module Main where
import qualified Data.Map as M
import Data.List
import Data.List.Split
import Control.Applicative

type Row = Integer
type Col = Integer
type Power = Integer

data Board = Board { numCols :: Int
				   , numRows :: Int
				   , board :: M.Map (Row, Col) Power
				   }

type DimInd = Integer
type NBoard = M.Map [DimInd] Power

data Dimension = Dim DimInd Dimension | DimEnd DimInd deriving (Eq, Ord, Show)

coords :: [Integer] -> [Dimension]
coords sizes = foldr toDim (makeEnds . head $ dims) $ tail dims
  where
    dims = map (\s -> [1..s]) sizes
    makeEnds = map DimEnd
    toDim ss dims = Dim <$> ss <*> dims

{-
coords []       = error "wtf mate"
coords (x:y:[]) = DimEnd x y 
coords (x:ys)   = Dim x . coords $ ys

-}



data Direction = LEFT | RIGHT | UP | DOWN

{-{{{
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
		b  = M.fromList . concatMap toCoord . zip [1..] . map (zip [1..]) $ l
		toCoord (r, cs) = map (\(c, v) -> ((r, c), v))  cs

toListOfCols :: Board -> [[Power]]
toListOfCols b = transpose . toListOfRows $ b

highestPower :: Board -> Power
highestPower = maximum . M.elems . board

toString :: Board -> String
toString b = intercalate "\n" $ [top] ++ guts ++ [top]
	where
		guts = map formatRow . toListOfRows $ b
		top  = "+" ++ replicate ((length . head $ guts) - 2) '-' ++ "+"

formatRow :: [Power] -> String
formatRow r = "| " ++ nums ++ " |"
	where
		nums = unwords . map formatNumber $ r

formatNumber :: Power -> String
formatNumber 0 = padToLength 2 " "
formatNumber n = padToLength 2 . show $ n

padToLength :: Int -> String -> String
padToLength n s = 
	if length s < n 
		then
			replicate (n - length s) ' ' ++ s
		else
			s

main :: IO ()
main = do
	let x = [[1,0,0], [1,2,0], [3,2,1]]
	let	b = fromListOfRows x
	putStrLn . toString $ b
}}}-}

main = print "hello"
