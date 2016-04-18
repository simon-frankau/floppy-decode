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
  putStrLn $ show $ map numberify $ byteify $ unMFM $ getSync $ toBitPattern $
             toBaseBitRate transitions
