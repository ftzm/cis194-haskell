module Golf where
import Data.List
import Data.List.Split.Internals
import Safe



skips :: [a] -> [[a]]
skips xs = map (lastDef [] . transpose. flip chunksOf xs) [1..length xs]

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:r)
  | y > x && y > z = y : localMaxima (y:z:r)
  | otherwise = localMaxima (y:z:r)
localMaxima _ = []

histogram :: [Integer] -> String
histogram xs = unlines $ transpose bars ++ ["==========", "0123456789"]
  where
    nums = foldr (\x acc -> length ( filter (==x) xs ) : acc) [] [0..9]
    longest = maximum nums
    bars = map (reverse . \x -> concat $ replicate x "*" ++ replicate (longest-x) " ") nums



a :: [Integer]
a = [1,4,5,4,6,6,3,4,2,4,9]

main :: IO ()
main = putStrLn $ histogram a
