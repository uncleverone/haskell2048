module Main where
import qualified Data.Map as M
import qualified System.Random as RNG
import Data.List
import Data.List.Split

type Row = Integer
type Col = Integer
type Power = Integer

data Board = Board { numCols :: Int
				   , numRows :: Int
				   , board :: (M.Map (Row, Col) Power)	
				   }

data NBoard = NBoard { size :: [Int]
					 , board :: (M.Map [Int] Power)
					 }

data NBoard a = Dim [Dim a] | Bottom a deriving Show

data Direction = LEFT | RIGHT | UP | DOWN

randomlyPlaceTile :: Board -> Board
randomlyPlaceTile = undefined

moveBoard :: Direction -> Board -> Board
moveBoard = undefined

toListOfRows :: Board -> [[Power]]
toListOfRows b = chunksOf (numCols b) . map snd . M.toAscList $ board b

fromListOfRows :: [[Power]] -> Board
fromListOfRows l = Board nc nr b
	where
		nc = length . head $ l
		nr = length l
		b  = M.fromList . concat . map toCoord . zip [1..] . map (zip [1..]) $ l
		toCoord (r, cs) = map (\(c, v) -> ((r, c), v)) $ cs

toListOfCols :: Board -> [[Power]]
toListOfCols b = transpose . toListOfRows $ b

highestPower :: Board -> Power
highestPower = maximum . M.elems . board

toString :: Board -> String
toString b = intercalate "\n" $ [top] ++ guts ++ [top]
	where
		guts = map formatRow . toListOfRows $ b
		top  = "+" ++ (replicate ((length . head $ guts) - 2) '-') ++ "+"

formatRow :: [Power] -> String
formatRow r = "| " ++ nums ++ " |"
	where
		nums = intercalate " " . map formatNumber $ r

formatNumber :: Power -> String
formatNumber 0 = padToLength 2 " "
formatNumber n = padToLength 2 . show $ n

padToLength :: Int -> String -> String
padToLength n s = 
	if length s < n 
		then
			(replicate (n - length s) ' ') ++ s
		else
			s

main = do
	let x = [[1,0,0], [1,2,0], [3,2,1]]
	let	b = fromListOfRows x
	putStrLn . toString $ b
