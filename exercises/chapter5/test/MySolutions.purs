module Test.MySolutions where

import Prelude

import ChapterExamples (Amp(..), Volt(..))
import Data.Array (foldr)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Picture (Bounds, Picture, Point, Shape(..), emptyBounds, intersect, origin, shapeBounds, union)
import Data.Picture as P
import Math (pi)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial  (n-1)

binomial :: Int->Int->Int
binomial n k | k == 0 = 1
             | k > n = 0
             | otherwise = (factorial n) / (factorial k * factorial (n-k))

pascal:: Int->Int->Int
pascal n k | k == 0 = 1
           | k > n = 0
           | otherwise = pascal (n-1) k + pascal (n-1) (k-1)


sameCity::forall r.forall a. {address::{city::String|a}|r}->{address::{city::String|a}|r}->Boolean
sameCity {address:{city:c1}} {address:{city:c2}} = c1 == c2

fromSingleton::forall a.a -> Array a -> a
fromSingleton _ [a] = a
fromSingleton a _ = a

circleAtOrigin :: Shape
circleAtOrigin = Circle origin 10.0

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter s = case s of
  Circle _ r -> Circle origin $ 2.0 * r
  Rectangle _ w h -> Rectangle origin (2.0*w) (2.0*h)
  Text _ t -> Text origin t
  Line {x:x1,y:y1} {x:x2,y:y2} -> Line {x: -dx,y: -dy} {x:dx,y:dy}  
    where dx = abs $ x2 - x1
          dy = abs $ y2 - y1 

shapeText :: Shape -> Maybe String
shapeText (Text _ t) = Just t
shapeText _ = Nothing

newtype Watt = Watt Number
calculateWattage:: Amp -> Volt -> Watt
calculateWattage (Amp c)  (Volt v) = Watt $ c * v

area :: Shape -> Number
area (Circle _ r) = pi * r * r
area (Rectangle _ w h) = w * h
area (Line _ _) = 0.0
area (Text _ _) = 0.0

data ShapeExt = Clipped Picture Point Number Number | Shape Shape

shapeBounds :: ShapeExt -> Bounds
shapeBounds (Clipped pic c w h) = intersect picb clip
  where picb = foldr union emptyBounds $ P.shapeBounds <$> pic
        clip = P.shapeBounds $ Rectangle c w h
shapeBounds (Shape s) = P.shapeBounds s
  