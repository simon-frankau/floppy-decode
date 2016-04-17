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

stat str xs =
  putStrLn $ str ++ ": " ++ (show $ minimum xs) ++ " " ++ (show $ maximum xs)

main = do
  -- Get the raw data...
  content <- filter (/= '\r') <$> readFile "floppy.csv"
  let lineData = words' <$> (drop 2 $ lines content)
  -- We only care about the data channel, and want it as doubles...
  let doubleData = (read . (!! 2)) <$> lineData :: [Double]
  let transitions = transitionTimes $ denoise doubleData
  -- Get stats on the transitions - they should group nicely...
  let fast = filter (< 125) transitions
  let medium = filter (\x -> x >= 125 && x < 175) transitions
  let slow = filter (>= 175) transitions
  stat "Fast" fast
  stat "Medium" medium
  putStrLn $ "Slows " ++ show slow
