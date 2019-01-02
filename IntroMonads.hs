import Text.Read
import Data.Char
import Control.Applicative
import Data.Foldable

sumIO = do
	putStrLn "Type two numbers"
	n1 <- readMaybe <$> getLine
	n2 <- readMaybe <$> getLine
	case (+) <$> n1 <*> n2 :: Maybe Double of
		Nothing -> putStrLn "Ivalid Numbers"
		Just x -> putStrLn $ "Got the sum-> "++show x
concatenator = do
	str <- (++) <$> getLine <*> getLine
	putStrLn str

data Expr = Value Int | Div (Expr) (Expr)

instance Show Expr where
	show (Value n) = show n
	show (Div x y) = "(" ++ show x ++ "/" ++ show y ++ ")"


safeDiv :: Int -> Int -> Maybe Int
safeDiv x y = if y == 0 then Nothing else Just (div x y)

eval :: Expr -> Maybe Int
eval (Value n) = return n
eval (Div x y) = eval x >>= (\ n -> eval y >>= (\ m -> safeDiv n m))

instance Eq Expr where
	Value n == Value m = n == m
	Value n == (Div x y) = return n == eval (Div x y)
	(Div x y) == Value n = eval (Div x y) == return n
	(Div x y) == (Div z t) = eval (Div x y) == eval (Div z t)

type Key = String
type Message = String
type Encrypt = Bool
--if encrypt (:: Encrypt) is true the function will encrypt
--if encrypt (:: Encrypt) is false the function will decrypt

vigerne :: Key -> Message -> Encrypt -> Message
vigerne key@(x:xs) msg@(y:ys) encrypt = go (take (length msg) (cycle key)) msg (if encrypt then 1 else -1) 
	where
		go _ "" _ = ""
		go (n:ns) (m:ms) mult = let sum = (ord m) + mult*(ord n) in
			(chr $ mod sum 128) : go ns ms mult

data Phone = Phone {name::String, phone::Int} deriving (Show, Eq)
findPhone :: [Phone] -> String -> Maybe Int
findPhone [] _ = Nothing
findPhone (x:xs) n = if name x == n then Just $ phone x else findPhone xs n

replicator [] n = []
replicator (x:xs) n = ((replicate n x):replicator xs n)

vigIO=do
	msg <- putStr "Tell me the message--> " >> getLine
	key <- putStr "Tell me the key--> " >> getLine
	encryptor <- putStr "Press 0 to encrypt, anything else to decrypt--> " >> (read <$> getLine) :: IO Int
	if encryptor == 0 then return $ vigerne msg key True else return $ vigerne msg key False

readDigit :: Int -> Char -> Maybe Int
readDigit i c 
	| i<0 || i>9 = Nothing
	| True = if [c] == show i then Just i else Nothing

binChar :: Char -> Maybe Int
binChar s = readDigit 1 s <|> readDigit 0 s

binParser s = asum $ sequence $ map binChar s
binParserIO = getLine >>= return.binParser

newtype Summ a = Summ {getSum :: a} deriving (Eq, Ord, Show)
instance Num a => Semigroup (Summ a) where
	(<>) (Summ a) (Summ b) = Summ $ a+b
instance Num a => Monoid (Summ a) where
	mempty = Summ 0
	mappend = (<>)