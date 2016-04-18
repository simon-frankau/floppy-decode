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

-- Group into bytes
byteify :: [Bool] -> [[Bool]]
byteify [] = []
byteify xs = b : byteify rest where (b, rest) = L.splitAt 8 xs

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

skipGap :: [Integer] -> Either String [Integer]
skipGap = return . dropWhile (== 0x4e)

skipGapAndSync :: [Integer] -> Either String [Integer]
skipGapAndSync xs = do
  xs' <- skipGap xs
  let (sync, rest) = splitAt 12 xs'
  if sync /= replicate 12 0x00
   then Left $ "Expected sync, got " ++ show sync
   else return rest

skipHeader :: [Integer] -> Either String [Integer]
skipHeader xs = do
  xs' <- skipGapAndSync xs
  let (iam, rest) = splitAt 4 xs'
  if iam /= [0xc2, 0xc2, 0xc2, 0xfc]
   then Left $ "Expected IAM, got " ++ show iam
   else return rest

readSectorHeader :: [Integer] -> Either String (String, [Integer])
readSectorHeader xs = do
  xs' <- skipGapAndSync xs
  let (idam, rest) = splitAt 4 xs'
  if idam /= [0xa1, 0xa1, 0xa1, 0xfe]
   then Left $ "Expected IDAM, got " ++ show idam
   else do
     let (sectorId, rest') = splitAt 4 rest
     -- Skip CRC
     return (show sectorId, drop 2 rest')

readSectorBody :: [Integer] -> Either String ([Integer], [Integer])
readSectorBody xs = do
  xs' <- skipGapAndSync xs
  let (dam, rest) = splitAt 4 xs'
  if dam /= [0xa1, 0xa1, 0xa1, 0xfb] && dam /= [0xa1, 0xa1, 0xa1, 0xf8]
   then Left $ "Expected DAM, got " ++ show dam
   else do
     let (content, rest') = splitAt 512 rest
     -- Skip CRC
     return (content, drop 2 rest')

readImage :: [Integer] -> Either String [(String, [Integer])]
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
  let byteStream = map numberify $ byteify $ unMFM $ getSync $ toBitPattern $
                   patchTiming $ toBaseBitRate transitions
  putStrLn $ show $ toBaseBitRate transitions
  putStrLn $ show $ byteStream
  putStrLn $ show $ readImage byteStream
  let Right [(_, blah)] = readImage byteStream
  writeBinary blah