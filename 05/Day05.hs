import Text.ParserCombinators.ReadP

import System.Environment (getArgs)
import Text.Printf (printf)

import Data.Char (isDigit)
import Data.List (sortBy)

type Page = Int
data Dependency = Dependency {before, after :: Page}
type Update = [Page]

type Input = ([(Page, Page)], [Update])

parse :: ReadP Input
parse = do
    deps <- dependencies
    newline
    newline
    ups <- updates
    return (map (\d -> (before d, after d)) deps, ups)
  where
    newline = char '\n'
    decimal = read <$> munch1 isDigit
    dep = Dependency <$> (decimal <* char '|') <*> decimal
    dependencies = dep `sepBy` newline
    update = decimal `sepBy` char ','
    updates = update `sepBy` newline

compareBySpec :: [(Int, Int)] -> Int -> Int -> Ordering
compareBySpec spec a b
    | a == b = EQ
    | (a, b) `elem` spec = LT
    | (b, a) `elem` spec = GT
    | otherwise = EQ

sortWithSpec :: [(Int, Int)] -> [Int] -> [Int]
sortWithSpec spec = sortBy (compareBySpec spec)

middle :: [a] -> a
middle [] = error "empty list"
middle xs = xs !! (length xs `div` 2)

part1 :: Input -> Int
part1 (spec, ups) = sum . map middle . filter correct $ ups
  where
    correct update = sortWithSpec spec update == update

part2 :: Input -> Int
part2 (spec, ups) = sum . map (middle . sortWithSpec spec) . filter (not . correct) $ ups
  where
    correct update = sortWithSpec spec update == update

main :: IO ()
main = do
    [filename] <- getArgs
    input <- fst . last . readP_to_S parse <$> readFile filename
    printf "Part 1: %d\n" $ part1 input
    printf "Part 2: %d\n" $ part2 input

-- Part 1: 5208
-- Part 2: 6732
