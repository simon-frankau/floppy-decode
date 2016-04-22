module Main where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Writer.Lazy
import qualified Data.List as L

------------------------------------------------------------------------
-- Transform the data into a bitstream
--

words' :: String -> [String]
words' s = case dropWhile (== ',') s of
             "" -> []
             s' -> w : words' s''
               where
                 (w, s'') = break (== ',') s'

-- Back of the envelope calculation:
-- One revolution in 200ms
-- Each track holds 1.44MB / 80 actual data.
-- That's around 18.8KB per track.
-- Or, with a pile of encoding overhead, 377Kb per track
-- -> Expect 2MHz or so.
--
-- TODO: Calc properly, with real-world overheads
-- Also, I've forgotten disks are double-sided...

-- These numbers come from looking at the data...
highHyst :: Double
highHyst = 0.3
lowHyst :: Double
lowHyst = 0.08

-- Smooth the sequences of samples, eliminate point noise stuff
denoise :: [Double] -> [Double]
denoise (x:y:rest) = (x + y)/2 : denoise rest
denoise x = x

-- Given a stream of numbers, and a high and low hysteresis point,
-- find the time between low transitions.
transitionTimes :: [Double] -> [Integer]
transitionTimes = tt 0 False
  where
    tt time True (x : xs) | x < lowHyst =
      time : tt 0 False xs
    tt time True (x : xs) | otherwise =
      tt (time + 1) True xs
    tt time False (x : xs) | x < highHyst =
      tt (time + 1) False xs
    tt time False (x : xs) | otherwise =
      tt (time + 1) True xs
    tt _ _ [] =
      []

-- Print the extrema in the list of data
stat :: String -> [Integer] -> IO ()
stat str xs =
  putStrLn $ str ++ ": " ++ (show $ minimum xs) ++ " " ++ (show $ maximum xs)

-- The bit rate comes from staring at the data...
bitRate :: Integer
bitRate = 50 -- Very convenient.

-- Print the timing stats for the transitions, make sure the buckets
-- are nice and distinct...
printBitStats :: [Integer] -> IO ()
printBitStats transitions = do
  let fast = filter (< 125) transitions
  let medium = filter (\x -> x >= 125 && x < 175) transitions
  let slow = filter (>= 175) transitions
  stat "Fast" fast
  stat "Medium" medium
  putStrLn $ "Slows " ++ show slow

-- Convert the timings to distinct multiples of the bit rate
toBaseBitRate :: [Integer] -> [Integer]
toBaseBitRate = map (\x -> (x + bitRate `div` 2) `div` bitRate)

-- Convert the timings into an actual bit pattern...
toBitPattern :: [Integer] -> [Bool]
toBitPattern = concatMap (\i -> True :
                                (take (fromIntegral $ i - 1) $ repeat False))

------------------------------------------------------------------------
-- Interpret the bit stream
--

-- Convenience...
prettyBits :: [Bool] -> String
prettyBits = map (\x -> if x then '1' else '0')

-- Remove the sync bits, un-MFMing the data stream
unMFM :: [Bool] -> [Bool]
unMFM (x:_:xs) = x : unMFM xs
unMFM xs = xs

-- Insert sync bits. Doesn't put a sync bit at the end, as we don't
-- know what's next.
addMFM :: [Bool] -> [Bool]
addMFM [] = []
addMFM [x] = [x]
addMFM (False : rest@(False : _)) = False : True : addMFM rest
addMFM (x : rest) = x : False : addMFM rest

-- Make a bit stream into a number
numberify :: [Bool] -> Integer
numberify = L.foldl' (\x y -> x * 2 + fromIntegral (fromEnum y)) 0

-- And a number into a stream
denumberify :: Integer -> [Bool]
denumberify = L.reverse . take 8 .
              L.unfoldr (\x -> Just (x `mod` 2 == 1, x `div` 2))

-- Gap 4A = 80 bytes 0x4E
-- Sync = 12 bytes 0x00
-- IAM = 4 bytes: C2 C2 C2 FC
-- GAP1 = 50 bytes 4E
-- Sector:
-- Sync = 12 bytes 0x00
-- IDAM = 4 bytes A1 A1 A1 FE
-- ID = 4 bytes of cylinder, head, sector, sector-size
-- CRC = 2 bytes
-- GAP 2 = 22 bytes of 4E
-- Sync = 12 bytes 00
-- DAM = 4 bytes A1 A1 A1 F[B8]
-- 512 bytes of data
-- CRC = 2 bytes
-- GAP 3 = 80 bytes 4E

data Log = Note String
           deriving (Show, Eq, Ord)

-- State holds the bit stream, ExceptT handles errors, Writer holds
-- the log of events...
type DiskM = StateT [Bool] (ExceptT String (Writer [Log]))

record :: Log -> DiskM ()
record = lift . lift . tell . (:[])

readBits :: Int -> DiskM [Bool]
readBits i = state $ splitAt i

finished :: DiskM Bool
finished = null <$> get

readInt :: DiskM Integer
readInt = (numberify . unMFM) <$> readBits 16

readInts :: Integer -> DiskM [Integer]
readInts i = sequence $ replicate (fromInteger i) readInt

tryRead :: Integer -> DiskM Bool
tryRead expect = do
  oldState <- get
  i <- readInt
  if i == expect
   then return True
   else do
     put oldState
     return False

expect :: String -> [Integer] -> DiskM ()
expect what (e:es) = do
  val <- readInt
  if val == e
   then expect what es
   else lift . throwE $ "Expected " ++ what ++ ", got " ++
                         show val ++ " instead of " ++ show e
expect _ [] = return ()

-- Search for the start of the gap.
findGap :: DiskM ()
findGap = do
  b <- tryRead 0x4E
  if b
   then do
     record $ Note "Found sync"
     return ()
   else do
     record $ Note "Skipping one bit"
     readBits 1
     findGap

-- Read through the gap.
skipGap :: DiskM ()
skipGap = do
  succeeded <- tryRead 0x4E
  if succeeded
   then skipGap
   else return ()

skipGapAndSync :: DiskM ()
skipGapAndSync = do
  skipGap
  expect "sync" (replicate 12 0x00)

skipHeader :: DiskM ()
skipHeader = do
  skipGap -- TODO: Sync
  skipHeader'
    where
      skipHeader' = do
        b <- tryRead 0xc2
        if b
         then expect "IAM" [0xc2, 0xc2, 0xfc]
         else do
           record $ Note "Finding IAM"
           readBits 1
           skipHeader'

readSectorHeader :: DiskM String
readSectorHeader = do
  skipGap -- TODO: Sync
  readSectorHeader'
    where
      readSectorHeader' = do
        b <- tryRead 0xa1
        if b
          then do
            expect "IDAM" [0xa1, 0xa1, 0xfe]
            sectorId <- readInts 4
            -- Skip CRC
            readInts 2
            record $ Note $ "Read sector head for sector " ++ show sectorId
            return $ show sectorId
        else do
          record $ Note "Finding IDAM"
          readBits 1
          readSectorHeader'

readSectorBody :: DiskM [Integer]
readSectorBody = do
  skipGap -- TODO: Sync
  readSectorBody'
    where
      readSectorBody' = do
        b <- tryRead 0xa1
        if b
          then do
            expect "DAM" [0xa1, 0xa1]
            b <- readInt
            if b /= 0xfb && b /= 0xf8
             then lift . throwE $ "Expected DAM, got " ++ show b
             else do
               res <- readInts 512
               record $ Note "Read sector body"
               -- Skip CRC
               readInts 2
               return res
          else do
            record $ Note "Finding DAM"
            readBits 1
            readSectorBody'

readImage :: DiskM [(String, [Integer])]
readImage = do
  findGap
  skipHeader
  readImageAux
    where
      readImageAux = do
        f <- finished
        if f
         then return []
         else do
           hdr <- readSectorHeader
           content <- readSectorBody
           others <- readImageAux
           return $ (hdr, content) : others

writeBinary :: [Integer] -> IO ()
writeBinary xs = do
  writeFile "floppy.img" $ map (toEnum . fromInteger) xs

main = do
  -- Get the raw data...
  content <- filter (/= '\r') <$> readFile "floppy.csv"
  let lineData = words' <$> (drop 2 $ lines content)
  -- We only care about the data channel, and want it as doubles...
  let doubleData = (read . (!! 2)) <$> lineData :: [Double]
  let transitions = transitionTimes $ denoise doubleData
  -- Get stats on the transitions - they should group nicely...
  printBitStats transitions
  -- And print the data
  let bitStream = toBitPattern $ toBaseBitRate transitions
  putStrLn $ show $ toBaseBitRate transitions
  putStrLn $ show $ prettyBits bitStream
  putStrLn $ show $ runWriter $ runExceptT $ runStateT readImage bitStream
  let (Right [(_, blah)], msgs) = runWriter $ runExceptT $ evalStateT readImage bitStream
  writeBinary blah