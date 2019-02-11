import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Concurrent

data Direction = North | South | East | West deriving Eq
data RL = R | L deriving (Eq, Show)

data Ant = Ant {getX :: Int, getY :: Int, currentDir :: Direction } deriving (Eq)
data World = World {arr::[Int], width::Int, height::Int}

move :: Ant -> Direction -> Ant 
move a North = if getY a == h-1 then Ant (getX a) (getY a) North else Ant (getX a) ((getY a)+1) North
move a South = if getY a == 0 then Ant (getX a) (getY a) South else Ant (getX a) ((getY a)-1) South
move a East = if getX a == w-1 then Ant (getX a) (getY a) West else Ant ((getX a) +1) (getY a) East
move a West = if getX a == 0 then Ant (getX a) (getY a) East else Ant ((getX a) -1) (getY a) West

rotate :: Direction -> RL -> Direction
rotate North d = if d == R then East else West
rotate East d = if d == R then South else North
rotate West d = if d == R then North else South
rotate South d = if d == R then West else East


step :: Ant -> Ant
step a = move a (currentDir a)

update :: World -> Int -> Int -> Int -> World
update (World xs w h) x y v = if x>=0 && y>=0 && x<w && y<h then World (take (y*w + x) xs ++ [v] ++ drop (y*w + x + 1) xs) w h else (World xs w h)

getEl :: World -> Int -> Int -> Int
getEl (World xs w h) x y = if x>=0 && y>=0 && x<w && y<h then xs !! (y*w + x) else -1 

clock :: Ant -> StateT World IO Ant
clock (Ant x y d) = state $ \ w -> if (getEl w x y) == 1 then (step $ Ant x y (rotate d R), update w x y 0) else (step $ Ant x y (rotate d L), update w x y 1)


h = 100
w = 100

inputW = World (replicate (h*w) 0) h w
inputA = Ant (div h 2) (div w 2) North

coolPrint :: World -> String
coolPrint (World [] _ _ ) = ""
coolPrint (World arr w h) = (toString $ take w arr) ++ "\n" ++ (coolPrint $ World (drop w arr) w h)

toString :: [Int] -> String
toString [] = ""
toString (x:xs) = ( if x==1 then "+" else " ") ++ " " ++ toString xs

updateAnt :: Ant -> StateT World IO Ant 
updateAnt ia = do
	lift $ threadDelay 10000
	lift $ putStrLn "\ESC[2J"
	s <- get
	lift $ putStrLn $ coolPrint s
	a <- clock ia
	updateAnt a

runGame = (runStateT (updateAnt inputA)) inputW
	

