import Control.Concurrent
import System.Random
import Control.Monad

data Cell = Cell {isAlive :: Bool, getX :: Int, getY :: Int} deriving Eq
data World = World {arr::[Cell], getW :: Int, getH :: Int} deriving Eq

instance Show Cell where
	show (Cell True _ _) = "o"
	show (Cell False _ _) = " "

getCell :: World -> Int -> Int -> Cell
getCell w x y = if x>=0 && y>=0 && x<(getW w) && y<(getH w) then (arr w) !! (y*(getW w) + x) else Cell False 0 0

getNeighbours :: World -> Cell -> Int
getNeighbours w (Cell _ x y) = length $ filter (isAlive) ( (uncurry $ getCell w) <$> [(x,y+1),(x,y-1),(x+1,y+1),(x+1,y),(x+1,y-1),(x-1,y+1),(x-1,y),(x-1,y-1)] )

newCell :: World -> Cell -> Cell
newCell w (Cell True x y) = let n = getNeighbours w (Cell True x y) in if n==2 || n==3 then Cell True x y else Cell False x y
newCell w (Cell False x y) = let n = getNeighbours w (Cell True x y) in if n==3 then Cell True x y else Cell False x y

clock :: World -> World
clock (World arr w h) = World ( (newCell (World arr w h)) <$> arr ) w h

buildCells :: [Int] -> Int -> Int -> Int -> Int -> [Cell]
buildCells input x y w h 
	| x==(w-1) && y==(h-1) = [Cell (if input !! (y*w+x) == 1 then True else False) x y]
	| x==(w-1) = [Cell (if input !! (y*w+x) == 1 then True else False) x y] ++ buildCells input 0 (y+1) w h
	| True = [Cell (if input !! (y*w+x) == 1 then True else False) x y] ++ buildCells input (x+1) y w h

buildWorld :: [Cell] -> Int -> Int -> World
buildWorld input w h = World input w h

width = 30
height = 20

input :: IO [Int]
input = replicateM (width*height) (randomRIO (0,1))

inputCells :: [Int] -> [Cell]
inputCells iArr = buildCells iArr 0 0 width height

inputWorld :: [Cell] -> World
inputWorld iCells = buildWorld iCells width height

coolPrint :: [Cell] -> Int -> String
coolPrint [] w = "----------------"
coolPrint arr w = (toString $ take w arr) ++ "\n" ++ coolPrint (drop w arr) w

toString :: [Cell] -> String
toString [] = ""
toString (x:xs) = show x ++ " " ++ toString xs

update w = do
	threadDelay 50000
	putStrLn "\ESC[2J"
	putStrLn $ coolPrint (arr w) width
	let nw = clock w
	if nw == w then putStrLn "Game Finished..." else update nw

runGame = do
	s <- input
	update $ inputWorld $ inputCells s