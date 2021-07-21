module Test.MySolutions where

import Prelude

import Control.MonadZero (guard)
import Data.Array (concatMap, filter, find, head, length, null, tail, (..), (:))
import Data.Foldable (class Foldable, foldl)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Path (Path(..), filename, isDirectory, ls, size)
import Test.Examples (allFiles, factorsV3)

-- Note to reader: Add your solutions to this file

isEven::Int->Boolean
isEven i = 
  if i == 0 then
    true
  else 
    if i > 0 then
        not $ isEven (i-1)
    else 
        not $ isEven (i+1)

countEven :: Array Int->Int
countEven a = 
  if null a then 0
  else cur + countEven rest
  where oneIfEven i = if isEven i then 1 else 0
        cur = maybe 0 oneIfEven $ head a
        rest = fromMaybe [] $ tail a


squared :: Array Number -> Array Number
squared = map (\x -> x * x)
keepNonNegative:: Array Number -> Array Number
keepNonNegative = filter \x -> x >=0.0

infixr 5 filter as <$?>

keepNonNegativeRewrite:: Array Number -> Array Number
keepNonNegativeRewrite a = (\x -> x >= 0.0) <$?> a

isPrime n = n /= 1 && 1 == length ( factorsV3 n)

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct a b = do 
  a1 <- a
  b1 <- b
  pure [a1 ,b1]

triples:: Int -> Array (Array Int)
triples n = do
  a <- 1..n
  b <- a..n
  c <- b..n
  guard $ c * c == a * a + (b * b)
  pure [a,b,c]

factorize:: Int->Array Int
factorize n = filter (\f-> mod n f == 0)$ filter isPrime (n..2)

allTrue:: Array Boolean -> Boolean
allTrue = foldl (&&) true 

fibTailRec :: Int -> Int
fibTailRec = go 1 1
  where go m1 m2 n = if n <= 1 then m1 else go (m1+m2) m1 (n-1)

reverse :: forall a. Array a -> Array a
reverse = foldl (\xs x -> [x] <> xs) []

onlyFiles :: Path -> Array Path
onlyFiles = allFiles >>> filter ( \x -> not $ isDirectory x)

whereIs :: Path -> String -> Maybe Path
whereIs d n = if isNothing loca  then subf  else Just d 
  where loca = find ((eq $ filename d <> n) <<< filename) $ ls d 
        subd = filter isDirectory $ ls d
        subf = fromMaybe Nothing $ head $ filter isJust $ (\dir -> whereIs dir n) <$> subd

largestSmallest ::Path->Array Path
largestSmallest a = if numf == 0 then []
                    else if numf == 1 then files else go f f files 
  where files = onlyFiles a
        numf = length files
        f = fromMaybe (File "" 0) $ head files
        fs x = fromMaybe 0 $ size x
        go mi ma fi = if isNothing nfo then [mi ,ma] else go nmi nma rest
          where nfo = head fi
                nf  = fromMaybe mi nfo
                rest = fromMaybe [] $ tail fi
                nmi = if fs nf < fs mi then nf else mi
                nma = if fs nf > fs ma then nf else ma                