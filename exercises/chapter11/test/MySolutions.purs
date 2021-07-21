module Test.MySolutions where

import Prelude

import Control.Monad.RWS (tell)
import Control.Monad.Reader (Reader, ask, local, runReader)
import Control.Monad.State (State, execState, modify)
import Control.Monad.Writer (Writer, runWriter)
import Data.Foldable (traverse_)
import Data.Int (even)
import Data.Monoid (power)
import Data.Monoid.Additive (Additive(..))
import Data.String (joinWith)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))

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
    addPar p s  = case s of
      Tuple _ false -> s
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
  