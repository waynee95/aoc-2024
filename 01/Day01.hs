import Data.List (sort)
import Data.Map qualified as M
import System.Environment (getArgs)
import Text.Printf (printf)

part1 :: ([Int], [Int]) -> Int
part1 (xs, ys) = sum $ zipWith (\x y -> abs (x - y)) (sort xs) (sort ys)

part2 :: ([Int], [Int]) -> Int
part2 (xs, ys) = sum . map (uncurry (*)) . M.toList $ M.intersectionWith (*) m1 m2
  where
    m1 = M.fromListWith (+) $ map (,1) xs
    m2 = M.fromListWith (+) $ map (,1) ys

main :: IO ()
main = do
    [filename] <- getArgs
    ls <- lines <$> readFile filename
    let (xs, ys) = unzip . map ((\[x, y] -> (read x, read y)) . words) $ ls
    printf "Part 1: %d\n" $ part1 (xs, ys)
    printf "Part 2: %d\n" $ part2 (xs, ys)

-- Part 1: 2285373
-- Part 2: 21142653
