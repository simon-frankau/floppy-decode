module Main where

import Control.Applicative
import qualified Data.List as L

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

-- Argh, I think there's a timing issue, possibly caused by
-- overwriting the sector?
patchTiming :: [Integer] -> [Integer]
patchTiming (2:2:6:xs) = xs
patchTiming (x:xs) = x : patchTiming xs
patchTiming [] = []

-- Convenience...
prettyBits :: [Bool] -> String
prettyBits = map (\x -> if x then '1' else '0')

-- Sync is 4E
-- That is, 01001110
-- With MFM, this becomes 0010010010101001
--                        . . . . . . . .

-- We'll look for the sync byte repeated 5 times, ignoring the fact that
-- it might appear in real data, etc.
syncPattern :: [Bool]
syncPattern = concat $ take 5 $ repeat oneSync where
  oneSync = map (== '1') "0010010010101001"

isSynced :: [Bool] -> Bool
isSynced xs = take (length syncPattern) xs == syncPattern

getSync :: [Bool] -> [Bool]
getSync = head . filter isSynced . L.tails

-- Remove the sync bits, un-MFMing the data stream
unMFM :: [Bool] -> [Bool]
unMFM (x:_:xs) = x : unMFM xs
unMFM xs = xs

-- Make a bit stream into a number
numberify :: [Bool] -> Integer
numberify = L.foldl' (\x y -> x * 2 + fromIntegral (fromEnum y)) 0


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

readInt :: [Bool] -> (Integer, [Bool])
readInt xs = (val, rest)
  where
    (bits, rest) = splitAt 16 xs
    val = numberify $ unMFM bits

readInts :: Integer -> [Bool] -> ([Integer], [Bool])
readInts 0 xs = ([], xs)
readInts i xs = (val : vals, rest')
  where
    (val, rest) = readInt xs
    (vals, rest') = readInts (i-1) rest

tryRead :: Integer -> [Bool] -> Maybe [Bool]
tryRead expect input =
  if val == expect then Just rest else Nothing
    where
      (val, rest) = readInt input

expect :: String -> [Integer] -> [Bool] -> Either String [Bool]
expect what (e:es) xs =
  if val == e
   then expect what es xs'
   else Left $ "Expected " ++ what ++ ", got " ++
        show val ++ " instead of " ++ show e
  where
    (val, xs') = readInt xs
expect _ [] xs = return xs

skipGap :: [Bool] -> Either String [Bool]
skipGap xs = case tryRead 0x4E xs of
               Just xs' -> skipGap xs'
               Nothing  -> return xs

skipGapAndSync :: [Bool] -> Either String [Bool]
skipGapAndSync xs = do
  xs' <- skipGap xs
  expect "sync" (replicate 12 0x00) xs'

skipHeader :: [Bool] -> Either String [Bool]
skipHeader xs = do
  xs' <- skipGapAndSync xs
  expect "IAM" [0xc2, 0xc2, 0xc2, 0xfc] xs'

readSectorHeader :: [Bool] -> Either String (String, [Bool])
readSectorHeader xs = do
  xs' <- skipGapAndSync xs
  xs'' <- expect "IDAM" [0xa1, 0xa1, 0xa1, 0xfe] xs'
  -- Skip CRC
  let (sectorId, rest) = readInts 4 xs''
  return (show sectorId, snd $ readInts 2 rest)

readSectorBody :: [Bool] -> Either String ([Integer], [Bool])
readSectorBody xs = do
  xs' <- skipGapAndSync xs
  rest <- expect "DAM" [0xa1, 0xa1, 0xa1] xs'
  let (b, rest') = readInt rest
  if b /= 0xfb && b /= 0xf8
   then Left $ "Expected DAM, got " ++ show b
   else do
     let (content, rest'') = readInts 512 rest'
     -- Skip CRC
     return (content, snd $ readInts 2 rest'')

readImage :: [Bool] -> Either String [(String, [Integer])]
readImage xs = do
  xs' <- skipHeader xs
  readImageAux xs'
    where
      readImageAux [] = return []
      readImageAux xs = do
        (hdr, xs') <- readSectorHeader xs
        (content, xs'') <- readSectorBody xs'
        others <- readImageAux xs''
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
  let bitStream = getSync $ toBitPattern $
                  patchTiming $ toBaseBitRate transitions
  putStrLn $ show $ toBaseBitRate transitions
  putStrLn $ show $ prettyBits bitStream
  putStrLn $ show $ readImage bitStream
  let Right [(_, blah)] = readImage bitStream
  writeBinary blah