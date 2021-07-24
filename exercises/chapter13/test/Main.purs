module Test.Main where

import Prelude

import Data.Array (fold, sort, sortBy)
import Data.Array.NonEmpty (cons', range, singleton)
import Data.Char (fromCharCode, toCharCode)
import Data.Foldable (foldMap, foldr)
import Data.Function (on)
import Data.List (List(..), fromFoldable)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe.First (First(..))
import Data.Show.Generic (genericShow)
import Data.Generic.Rep (class Generic)
import Data.Traversable (traverse)
import Effect (Effect)
import Merge (merge, mergePoly, mergeWith)
import Sorted (sorted)
import Test.QuickCheck (class Coarbitrary, Result(..), quickCheck, (<?>))
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary, coarbitrary)
import Test.QuickCheck.Gen (Gen, arrayOf, elements, oneOf)
import Tree (Tree, anywhere, insert, member, toArray)

isSorted :: forall a. (Ord a) => Array a -> Boolean
isSorted = go <<< fromFoldable
  where
  go (Cons x1 t@(Cons x2 _)) = x1 <= x2 && go t
  go _ = true

ints :: Array Int -> Array Int
ints = identity

bools :: Array Boolean -> Array Boolean
bools = identity

strings :: Array String -> Array String
strings = identity

intToBool :: (Int -> Boolean) -> Int -> Boolean
intToBool = identity

treeOfInt :: Tree Int -> Tree Int
treeOfInt = identity

newtype AzString = AzString String

instance arbitraryAz :: Arbitrary AzString where
  arbitrary:: Gen AzString
  arbitrary = 
    let chars = fromMaybe (singleton 'a') $ traverse fromCharCode $ range (toCharCode 'a') (toCharCode 'z')
        createAz = map show >>> fold >>> AzString
    in createAz <$> (arrayOf $ elements chars)


newtype Byte = Byte Int

intToByte :: Int -> Byte
intToByte n | n >= 0 = Byte (n `mod` 256)
            | otherwise = intToByte (-n)


instance arbitraryByte :: Arbitrary Byte where
  arbitrary = map intToByte arbitrary

instance coarbitraryByte :: Coarbitrary Byte where
  coarbitrary (Byte a) = coarbitrary <<< intToByte $ a

checkAssociative :: (Int -> Boolean) -> Array Int -> Array Int -> Array Int -> Result
checkAssociative f xs ys zs =
  let p1 =  mergeWith (intToBool f) xs ys
      p2 =  mergeWith f ys zs
      pre = mergeWith f p1 zs
      post = mergeWith f xs p2
  in pre == post <?> "Associativity failed for\n" <> show xs <> "\n" <> show ys <> "\n" <> show zs

data OneTwoThree a = One a | Two a a | Three a a a


instance arbitrary123 ::Arbitrary a => Arbitrary (OneTwoThree a) where
  arbitrary = oneOf $ cons' 
    (map One arbitrary) [
    map Two arbitrary <*> arbitrary
    ,map Three arbitrary <*> arbitrary <*> arbitrary]

instance coarbitrary123 ::Coarbitrary a => Coarbitrary (OneTwoThree a) where
  coarbitrary (One a) = coarbitrary a
  coarbitrary (Two a b) = coarbitrary  a <<< coarbitrary b
  coarbitrary (Three a b c) = coarbitrary  a <<< coarbitrary b <<< coarbitrary c
  
all:: List Result -> Boolean
all = foldr (&&) true <<< map toBool where
  toBool Success = true
  toBool _ = false

squashResults :: List Result -> Result
squashResults l = e where
  toErr Success = Nothing
  toErr x = Just x
  errors = toErr <$> l
  e = case foldMap First errors of
      First Nothing -> Success
      First (Just err) -> err

derive instance generic123 ::Generic (OneTwoThree a) _

instance show123 ::Show a => Show (OneTwoThree a) where
  show = genericShow

sum123 :: OneTwoThree Int -> OneTwoThree Int -> OneTwoThree Int
sum123 x y = One $ toInt x + toInt y where
  toInt (One a) = a
  toInt (Two a b) =  a + b
  toInt (Three a b c) =  a + b + c


instance one23Eq :: Eq a => Eq (OneTwoThree a) where
  eq (One a) (One b) = a == b
  eq (Two a b ) (Two c d) = a == c && b == d
  eq (Three a b c) (Three a1 b1 c1) = a == a1 && b == b1 && c == c1
  eq _ _ = false

type One23f = OneTwoThree Int -> OneTwoThree Int -> OneTwoThree Int

assoc123 :: One23f -> OneTwoThree Int -> OneTwoThree Int -> OneTwoThree Int -> Result
assoc123 f x y z = pre == post <?> "Associativity failed\n" <> show x <> "\n" <> show y <> "\n" <> show z where
  pre = f  (f x y) z
  post = f x (f y z) 
  

main :: Effect Unit
main = do
  -- Tests for module 'Merge'

  quickCheck \xs ys ->
    let
      result = merge (sort xs) (sort ys)
      expected = sort $ xs <> ys
    in
      eq result expected <?> "Result:\n" <> show result <> "\nnot equal to expected:\n" <> show expected

  quickCheck \xs ->
    let
      result = merge xs []
      expected = xs
    in
      eq result expected <?> "Result:\n" <> show result <> "\nnot equal to expected:\n" <> show expected

  quickCheck \xs ys ->
    let 
      result = merge (sorted xs) (sorted ys)
      expected = sort $ sorted xs <> sorted ys
    in
      eq result expected <?> "Result:\n" <> show result <> "\nnot equal to expected:\n" <> show expected

  quickCheck \xs ys ->
    let 
      result = ints $ mergePoly (sorted xs) (sorted ys)
      expected = sort $ sorted xs <> sorted ys
    in
      eq result expected <?> "Result:\n" <> show result <> "\nnot equal to expected:\n" <> show expected
  

  quickCheck \xs ys ->
    let 
      result = bools $ mergePoly (sorted xs) (sorted ys)
      expected = sort $ sorted xs <> sorted ys
    in
      eq result expected <?> "Result:\n" <> show result <> "\nnot equal to expected:\n" <> show expected
            
  quickCheck \xs ys f ->
    let
      result = map f $ mergeWith (intToBool f) (sortBy (compare `on` f) xs) (sortBy (compare `on` f) ys)
      expected = map f $ sortBy (compare `on` f) $ xs <> ys
    in
      eq result expected <?> "Result:\n" <> show result <> "\nnot equal to expected:\n" <> show expected

  -- Tests for module 'Tree'

  quickCheck \t a -> (member a $ insert a $ treeOfInt t) <?> "Inserted value:\n" <> show a <> "\nnot found in tree\n"
    

  quickCheck \t xs -> (isSorted $ toArray $ foldr insert t $ ints xs) <?> "Folding insert should return a sorted array\n" <> show xs

  quickCheck \f g t ->
    anywhere (\s -> f s || g s) t ==
      anywhere f (treeOfInt t) || anywhere g t

  -- exercise: quickcheck for fold
  quickCheck \xs ys ->
    let result = strings $ fold $ xs <> ys
        expected = fold xs <> fold ys
    in 
      eq result expected <?> "Result:\n" <> show result <> "\nnot equal to expected:\n" <> show expected

  -- exercise: quickcheck for tree insertions
  quickCheck \t a bs ->
    let ta = insert a t
        tb = foldr insert ta (ints bs)
    in member a tb <?> "Inserted value:\n" <> show a <> "\nnot found in tree after inserting\n" <> show bs
    