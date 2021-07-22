module Test.Main where

import Prelude

import Control.Monad.List.Trans (foldl)
import Data.Array (fold, sort, sortBy, (..))
import Data.Array.NonEmpty (NonEmptyArray(..), range, singleton)
import Data.Char (fromCharCode, toCharCode)
import Data.Foldable (foldr)
import Data.Function (on)
import Data.List (List(..), fromFoldable)
import Data.Maybe (fromMaybe)
import Data.Traversable (traverse)
import Effect (Effect)
import Merge (mergeWith, mergePoly, merge)
import Sorted (sorted)
import Test.QuickCheck (quickCheck, (<?>))
import Test.QuickCheck.Arbitrary (arbitrary, class Arbitrary)
import Test.QuickCheck.Gen (Gen, arrayOf, elements)
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
    