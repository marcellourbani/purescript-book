module Test.MySolutions where

import Data.Foldable
import Prelude

import Control.Applicative ((<$>))
import Data.Array (nub, nubByEq, nubEq,length)
import Data.Array.Partial (head)
import Data.Char (toCharCode)
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype, over2, wrap)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (toCharArray)
import Data.Hashable

-- Note to reader: Add your solutions to this file

newtype Point
  = Point
  { x :: Number
  , y :: Number
  }
instance showPoint :: Show Point where 
  show (Point {x,y}) = "(" <> show x <> ", " <>show y <> ")"

newtype Complex
  = Complex
  { real :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex {real,imaginary} )
    | imaginary >= 0.0 = show real <> "+" <> show imaginary <> "i"
    | otherwise = show real <> show imaginary <> "i"

derive instance eqComplex :: Eq Complex
derive instance ntComplex :: Newtype Complex _

instance semiComplex :: Semiring Complex where
  add = over2 Complex (+)
  zero = wrap zero
  one = wrap {real:one,imaginary:zero}
  mul = over2 Complex \{real:r1,imaginary:i1} {real:r2,imaginary:i2} -> {real:r1*r2 - (i1*i2),imaginary:i1*r2+(i2*r1)}

derive newtype instance grComplex :: Ring Complex 

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

derive instance genericShape :: Generic Shape _
instance showShape :: Show Shape where show = genericShow

data NonEmpty a = NonEmpty a (Array a)
instance  eqne :: Eq a => Eq (NonEmpty a) where
  eq (NonEmpty x xs) (NonEmpty y ys) = x == y && xs == ys
derive instance genericNE :: Generic (NonEmpty a) _
instance showNE :: Show a =>Show (NonEmpty a) where show = genericShow

instance semiNE :: Semigroup (NonEmpty a) where
  append (NonEmpty x xs) (NonEmpty y ys) = NonEmpty x ( xs <> [y] <> ys )

instance funcNE :: Functor NonEmpty where
  map f (NonEmpty x xs) = NonEmpty (f x) ( f <$> xs )

data Extended a = Infinite | Finite a

derive instance eqExtended :: Eq a => Eq (Extended a)
instance ordExtended :: Ord a => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare Infinite _ = GT
  compare _ Infinite = LT
  compare (Finite a) (Finite b) = compare a b

instance foldableNE :: Foldable NonEmpty where
  foldl f s (NonEmpty x xs) = foldl f s ([x] <> xs)
  foldr f s (NonEmpty x xs) = foldr f s ([x] <> xs)
  foldMap f (NonEmpty x xs) = foldMap f ([x] <> xs)

data OneMore f a = OneMore a (f a)

instance foldableOM :: Foldable f => Foldable (OneMore f) where
  foldr f s (OneMore x xs) = f x $ foldr f s xs
  foldl f s (OneMore x xs) = foldl f (f s x) xs
  foldMap f (OneMore x xs) = f x <> foldMap f xs

derive instance eqPoint :: Eq Point
derive instance eqShape :: Eq Shape
dedupShapes :: Array Shape -> Array Shape
dedupShapes = nubEq

derive instance ordPoint :: Ord Point
derive instance ordShape :: Ord Shape

dedupShapesFast :: Array Shape -> Array Shape
dedupShapesFast = nub

unsafeMaximum :: Partial => Array Int -> Int
unsafeMaximum a = fromMaybe (head a) $ maximum a

class Monoid m <= Action m a where
  act :: m -> a -> a

newtype Multiply = Multiply Int

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance monoidMultiply :: Monoid Multiply where
  mempty = Multiply 1

instance actionMultiplyInt :: Action Multiply Int where
  act (Multiply n) m = n * m

instance actionMultiplyString :: Action Multiply String where
  act (Multiply n) m = times m n where
    times a b | b < 1 = ""
              | otherwise = a <> times a (b - 1)

instance antionArray :: Action m a => Action m (Array a) where 
  act a t = act a <$> t

newtype Self m = Self m

derive newtype instance showSelf :: Show a => Show (Self a)
derive instance eqSelf :: Eq a => Eq (Self a)

derive newtype instance showMultiply :: Show Multiply
derive instance eqMultiply :: Eq Multiply

instance actSelf ::Monoid m => Action m (Self m) where
  act a (Self b) = Self $ a <> b


arrayHasDuplicates ::forall a.Hashable a => Eq a => Array a -> Boolean
arrayHasDuplicates a = length a /= length (nubByEq isEq a) 
  where isEq x y = hashEqual x y && x == y


newtype Hour = Hour Int

instance eqHour :: Eq Hour where
  eq (Hour n) (Hour m) = mod n 12 == mod m 12

instance hourHashable :: Hashable Hour where
  hash (Hour h) = hashCode $ mod h 12