module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Writer.Lazy
import qualified Data.List as L
import qualified Data.Map as M

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
highHyst = 0.2
lowHyst :: Double
lowHyst = 0.08

-- Smooth the sequences of samples, eliminate point noise stuff
denoise :: [Double] -> [Double]
denoise = map (minimum . take 5) . filter (not . null) . L.tails

-- Given a stream of numbers, and a high and low hysteresis point,
-- find the time between low transitions.
transitionTimes :: [Double] -> [Int]
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
stat :: String -> [Int] -> IO ()
stat str xs =
  putStrLn $ str ++ ": " ++ (show $ minimum xs) ++ " " ++ (show $ maximum xs)

-- The bit rate comes from staring at the data...
-- This is from a scan with a 5ms pitch on the 'scope.
bitRate :: Int
bitRate = 10

-- Print the timing stats for the transitions, make sure the buckets
-- are nice and distinct...
printBitStats :: [Int] -> IO ()
printBitStats transitions = do
  let fast = filter (< 125) transitions
  let medium = filter (\x -> x >= 125 && x < 175) transitions
  let slow = filter (>= 175) transitions
  stat "Fast" fast
  stat "Medium" medium
  putStrLn $ "Slows " ++ show slow

-- Convert the timings to distinct multiples of the bit rate
toBaseBitRate :: [Int] -> [Int]
toBaseBitRate = map (\x -> (x + bitRate `div` 2) `div` bitRate)

-- Convert the timings into an actual bit pattern...
toBitPattern :: [Int] -> [Bool]
toBitPattern = concatMap (\i -> True : (replicate (i - 1) False))

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
numberify :: [Bool] -> Int
numberify = L.foldl' (\x y -> x * 2 + fromEnum y) 0

-- And a number into a stream
denumberify :: Int -> [Bool]
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

peekBits :: Int -> DiskM [Bool]
peekBits i = take i <$> get

finished :: DiskM Bool
finished = null <$> get

readByte :: DiskM Int
readByte = (numberify . unMFM) <$> readBits 16

readBytes :: Int -> DiskM [Int]
readBytes i = sequence $ replicate i readByte

-- Peek bits, and read them if they're what we expect.
tryBits :: [Bool] -> DiskM Bool
tryBits xs = do
  let i = length xs
  ys <- peekBits i
  if xs == ys
   then do
     -- We also read the extra clock bit, which depends on following data.
     readBits $ i + 1
     return True
   else
     return False

bits :: String -> [Bool]
bits = map (== '1')

-- Special bit sequences with non-standard clocking.
xa1 = bits "100010010001001"
xc2 = bits "101001000100100"
-- Other constants
x4e = addMFM $ denumberify 0x4E
xfe = addMFM $ denumberify 0xFE
xfc = addMFM $ denumberify 0xFC

expect :: String -> [[Bool]] -> DiskM ()
expect what es = do
  b <- and <$> mapM tryBits es
  unless b $ lift . throwE $ "Failure to read "  ++ what

-- Search for the start of the gap.
syncTo :: [Bool] -> DiskM ()
syncTo bs = do
  found <- tryBits bs
  unless found $ do
    b <- readBits 1
    record $ Note $ "Skipping: " ++ show b
    syncTo bs

-- Read through the gap.
skipGap :: DiskM ()
skipGap = do
  succeeded <- tryBits x4e
  when succeeded $ do
    record $ Note "Read gap"
    skipGap

skipHeader :: DiskM ()
skipHeader = do
  skipGap
  syncTo xc2
  expect "IAM" [xc2, xc2, xfc]
  record $ Note "Read IAM"

readSectorHeader :: DiskM String
readSectorHeader = do
  skipGap
  syncTo xa1
  expect "IDAM" [xa1, xa1, xfe]
  record $ Note "Read IDAM"
  sectorId <- readBytes 4
  record $ Note $ "Sector id: " ++ show sectorId
  -- Skip CRC
  crc <- readBytes 2
  record $ Note $ "CRC: " ++ show crc
  return $ show sectorId

readSectorBody :: DiskM [Int]
readSectorBody = do
  skipGap
  syncTo xa1
  expect "DAM" [xa1, xa1]
  b <- readByte
  when (b /= 0xfb && b /= 0xf8) $
    lift . throwE $ "Expected DAM, got " ++ show b
  record $ Note "Read DAM"
  res <- readBytes 512
  record $ Note "Read sector body"
  -- Skip CRC
  crc <- readBytes 2
  record $ Note $ "CRC: " ++ show crc
  return res

readImage :: DiskM [(String, [Int])]
readImage = do
  syncTo x4e
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

writeBinary :: [Int] -> IO ()
writeBinary xs = do
  writeFile "floppy.img" $ map toEnum xs

main = do
  -- Get the raw data...
  content <- filter (/= '\r') <$> readFile "floppy2.csv"
  let lineData = words' <$> (drop 2 $ lines content)
  -- We only care about the data channel, and want it as doubles...
  let doubleData = denoise $ (read . (!! 1)) <$> lineData :: [Double]
  let transitions = transitionTimes doubleData
  let histogram = M.toList $ M.fromListWith (+) $ map (\x -> (x, 1)) transitions
  mapM_ (putStrLn . show) histogram
  -- mapM_ (putStrLn . show . (*1000))  doubleData
  -- mapM_ (putStrLn . show) transitions
  -- Get stats on the transitions - they should group nicely...
  -- printBitStats transitions
  -- And print the data
  let bitStream = toBitPattern $ toBaseBitRate transitions
  -- putStrLn $ show $ toBaseBitRate transitions
  putStrLn $ show $ prettyBits bitStream
  -- putStrLn $ show $ runWriter $ runExceptT $ runStateT readImage bitStream
  let (_, msgs) = runWriter $ runExceptT $ evalStateT readImage bitStream
  mapM (putStrLn . show) msgs
  -- writeBinary blah
