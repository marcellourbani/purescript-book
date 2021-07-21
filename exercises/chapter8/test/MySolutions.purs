module Test.MySolutions where

import Data.Tuple
import Prelude

import Control.Monad.List.Trans (filter)
import Control.Monad.ST (for, run)
import Control.Monad.ST.Ref (modify, new, read, write)
import Data.Array (foldM, head, nub, sort, tail)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (error, throwException)
import Math (pow)
import React.Basic.DOM.Components.Ref (ref)

-- Note to reader: Add your solutions to this file

third ::forall a. Array a -> Maybe a
third a = tail a >>=tail >>= head

possibleSums :: Array Int -> Array Int
possibleSums a = nub $ sort $ foldM (\acc i -> [acc,acc + i]) 0 a


filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
-- filterM f [] = pure []
filterM _ Nil = pure Nil
filterM f (Cons x xs) = cond <$> f x <*> filterM f xs where 
  cond y = if y then (Cons x) else identity

exceptionDivide :: Int->Int-> Effect Int
exceptionDivide _ 0 = throwException $ error "div zero"
exceptionDivide a b = pure $ a / b

estimatePi :: Int -> Number
estimatePi s = run do 
  ref <- new 0.0
  for 1 s \k -> modify (\acc -> let kf = toNumber k in acc + pow (-1.0) (kf+1.0) / (2.0*kf - 1.0) ) ref
  final <- read ref
  pure $ final * 4.0

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci n | n <= 2 = 1
            | otherwise = run do
              xr <- new 1
              yr <- new 1
              for 1 (n-1) \_ -> do
                x <- read xr 
                y <- read yr
                _ <- write y xr
                write (x+y) yr
              read yr

