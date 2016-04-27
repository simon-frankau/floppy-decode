module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Writer.Lazy
import           Data.Foldable
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

-- Smooth the sequences of samples, eliminate point noise stuff
denoise :: [Double] -> [Double]
denoise = map (minimum . take 5) . filter (not . null) . L.tails

-- Given a stream of numbers, and a high and low hysteresis point,
-- find the time between low transitions.
transitionTimes :: Double -> Double -> [Double] -> [Int]
transitionTimes lowHyst highHyst = tt 0 False
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
toBaseBitRate :: Int -> [Int] -> [Int]
toBaseBitRate bitRate = map (\x -> (x + bitRate `div` 2) `div` bitRate)

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

-- State holds the bit stream, ExceptT handles errors, Writer holds
-- the log of events...
type DiskM = StateT [Bool] (ExceptT String (Writer [String]))

record :: String -> DiskM ()
record = lift . lift . tell . (:[])

readBits :: Int -> DiskM [Bool]
readBits i = do
  bits <- state $ splitAt i
  record $ prettyBits bits
  return bits

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

glue :: [[Bool]] -> [Bool]
glue (x:xs@(_:_)) = x ++ (bit : xs')
  where
    bit = not (last x || head xs')
    xs' = glue xs
glue [xs] = xs
glue [] = []

-- Special bit sequences with non-standard clocking.
xa1 = bits "100010010001001"
xc2 = bits "101001000100100"
-- Other constants
x4e = addMFM $ denumberify 0x4E
xf8 = addMFM $ denumberify 0xF8
xfb = addMFM $ denumberify 0xFB
xfc = addMFM $ denumberify 0xFC
xfe = addMFM $ denumberify 0xFE

iam = glue [xc2, xc2, xc2, xfc]
idam = glue [xa1, xa1, xa1, xfe]
damF8 = glue [xa1, xa1, xa1, xf8]
damFB = glue [xa1, xa1, xa1, xfb]

expect :: String -> [[Bool]] -> DiskM ()
expect what es = do
  b <- and <$> mapM tryBits es
  unless b $ lift . throwE $ "Failure to read "  ++ what

-- Search for the start of the gap.
syncTo :: [Bool] -> DiskM ()
syncTo bs = do
  found <- tryBits bs
  end <- finished
  unless (found || end) $ do
    b <- readBits 1
    syncTo bs

readIAM :: DiskM ()
readIAM = do
  -- Track header...
  record "[Read IAM]"

readIDAM :: DiskM [Int]
readIDAM = do
  record "[Read IDAM]"
  sectorId <- readBytes 4
  record $ "[Sector id: " ++ show sectorId ++ "]"
  -- Skip CRC
  crc <- readBytes 2
  record $ "[CRC: " ++ show crc ++ "]"
  return sectorId

readDAM :: DiskM [Int]
readDAM = do
  record "[Read DAM]"
  res <- readBytes 512
  record "[Read sector body]"
  -- Skip CRC
  crc <- readBytes 2
  record $ "[CRC: " ++ show crc ++ "]"
  return res

data Item = Gap
          | Track
          | SectorHeader [Int]
          | SectorData [Int]
          | Dunno
            deriving (Show, Eq, Ord)

readItem :: DiskM Item
readItem = do
  val <- foldlM (\val (pattern, op) ->
              case val of
                Just x  -> return val
                Nothing -> do
                  found <- tryBits pattern
                  if found
                    then Just <$> op
                    else return Nothing) Nothing
           [(x4e,   record "[Read gap]" >> return Gap),
            (iam,   readIAM             >> return Track),
            (idam,  SectorHeader <$> readIDAM),
            (damF8, SectorData   <$> readDAM),
            (damFB, SectorData   <$> readDAM)]
  case val of
    Just x -> return x
    Nothing -> do
      readBits 1
      return Dunno

readImage :: DiskM [Item]
readImage = reverse <$> readImage' [] where
  readImage' items = do
    item <- readItem
    more <- not <$> finished
    if more
      then readImage' $ item : items
      else return items

writeBinary :: String -> [Int] -> IO ()
writeBinary fileName xs = do
  writeFile fileName $ map toEnum xs

-- My preferred tools don't like really long lines. Use sane line lengths.
printLog :: [String] -> IO ()
printLog log = do
  let bigLog = concat log
  let chunks = takeWhile (not . null) $ L.unfoldr (Just . splitAt 72) bigLog
  mapM_ putStrLn chunks

process :: String -> String -> Int -> Double -> Double -> Int -> IO ()
process inName outName column lowHyst highHyst bitRate = do
  -- Get the raw data...
  content <- filter (/= '\r') <$> readFile inName
  let lineData = words' <$> (drop 2 $ lines content)
  -- We only care about the data channel, and want it as doubles...
  let doubleData = denoise $ (read . (!! column)) <$> lineData :: [Double]
  let transitions = transitionTimes lowHyst highHyst  doubleData
  -- Print histogram of cycle times, from which we can bind an appropriate
  -- value of bitRate
  putStrLn "Pulse length histogram:"
  let histogram = M.toList $ M.fromListWith (+) $
                  map (\x -> (x, 1)) transitions
  mapM_ (putStrLn . show) histogram
  -- Print the data
  putStrLn "Source data:"
  let bitStream = toBitPattern $ toBaseBitRate bitRate transitions
  putStrLn $ prettyBits bitStream
  -- And decode it...
  putStrLn "Interpreted data:"
  -- putStrLn $ show $ runWriter $ runExceptT $ runStateT readImage bitStream
  -- let (Right [(_, sect)], msgs) = runWriter $ runExceptT $ evalStateT readImage bitStream
  let (blah, msgs) = runWriter $ runExceptT $ evalStateT readImage bitStream
  printLog msgs
  putStrLn $ show blah
  -- writeBinary outName sect

main = do
  -- Old file.
  -- process "floppy.csv" "floppy.img" 2 0.08 0.3 100
  -- Process a file with a 5ms pitch on the 'scope.
  process "floppy2.csv" "floppy2.img" 1 0.08 0.2 10
