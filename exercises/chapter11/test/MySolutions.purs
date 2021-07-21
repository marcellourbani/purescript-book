module Test.MySolutions where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.RWS (tell)
import Control.Monad.Reader (Reader, ReaderT, ask, local, runReader, runReaderT)
import Control.Monad.State (State, execState, get, modify, put)
import Control.Monad.Writer (Writer, WriterT, runWriter, runWriterT)
import Data.Array (fold, intercalate, some)
import Data.Either (Either)
import Data.Foldable (traverse_)
import Data.Identity (Identity)
import Data.Int (even)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.String (Pattern(..), joinWith, stripPrefix)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Split (Parser)

-- Note to reader : Add your solutions to this file
testParens :: String -> Boolean
testParens s = valid $ (execState do go s) (Tuple 0 true) where
    valid (Tuple c v) = v && c==0
    go :: String -> State (Tuple Int Boolean) Unit
    go x = traverse_ countPar $ toCharArray x
    countPar c = modify $ addPar $ parToInt c
    parToInt c = case c of 
                      '(' -> 1
                      ')' -> - 1
                      _   -> 0

    addPar :: Int -> Tuple Int Boolean -> Tuple Int Boolean
    addPar p q  = case q of
      Tuple _ false -> q
      Tuple n true -> if p == -1 && n == 0 then Tuple 0 false else Tuple (n+p) true
       
type Level = Int
type Doc = Reader Level String

line :: String -> Doc
line s = do
  level <- ask
  pure $ power "  " level <> s

indent :: Doc -> Doc
indent = local \l->l+1

cat :: Array Doc -> Doc
cat ls = joinWith "\n" <$> sequence ls

render :: Doc -> String
render d = runReader d 0

sumArrayWriter :: Array Int -> Writer ( Additive Int) Unit
sumArrayWriter = traverse_ \n -> tell $Additive n

collatz :: Int -> Tuple Int (Array Int)
collatz x = runWriter $ go 0 x where
  go c 1 = do tell [1]
              pure c
  go c n = do tell [n]
              go  (c+1) if even n then n / 2 else 3*n+1
  

safeDivide :: Number -> Number -> Either String Number
safeDivide x y = unwrap $ runExceptT $ go x y where
  go:: Number -> Number -> ExceptT String Identity Number
  go a b = if b == 0.0 then throwError "Divide by zero" else pure $ a / b

string :: String -> Parser String
string pre = do
  s <- get
  tell ["The state is " <> s]
  case (stripPrefix (Pattern pre) s) of
    Just rest -> do 
       put rest
       pure pre
    Nothing -> throwError ["Could not parse"]

type Doc' = ReaderT Int (WriterT (Array String) Identity) Unit
indent' ::Doc' -> Doc'
indent' = local \c ->  c + 1

line' ::  String -> ReaderT Int (WriterT (Array String) Identity) Unit
line' s = do
  level <- ask
  tell $ [power "  " level <> s]
  pure unit


render' :: Doc' -> String
render' x = case ( unwrap $ runWriterT $ runReaderT x 0 ) of Tuple _ a -> intercalate "\n" a

asandthenbs ::  Parser String
asandthenbs = do
  as <- some $ string "a"
  bs <- some $ string "b"
  pure $ fold $ as <> bs

asorbs :: Parser String
asorbs = fold <$> some (string "a" <|> string "b")