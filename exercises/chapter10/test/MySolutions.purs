module Test.MySolutions where

import Prelude

import Control.Alt (alt, (<|>))
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson, jsonParser)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Function.Uncurried (Fn3)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Pair (Pair(..))
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Test.Examples (Complex, Quadratic, Undefined, isUndefined)

-- Note to reader: Add your solutions to this file

foreign import volumeFn :: Fn3 Number Number Number Number
foreign import volumeArrow :: Number -> Number -> Number -> Number
foreign import cumulativeSumsComplex :: Array Complex -> Array Complex
foreign import quadraticRootsImpl ::(forall a. a->a-> Pair a)-> Quadratic ->Pair Complex

quadraticRoots:: Quadratic -> Pair Complex
quadraticRoots = quadraticRootsImpl Pair

foreign import valuesOfMapJ:: Json -> Json
valuesOfMap :: Map String Int -> Either JsonDecodeError (Set Int)
valuesOfMap = encodeJson >>> valuesOfMapJ >>> decodeJson

valuesOfMapGeneric :: forall k v.Ord v=>Ord k=> EncodeJson k =>EncodeJson v =>DecodeJson v=> Map k v -> Either JsonDecodeError (Set v)
valuesOfMapGeneric = encodeJson >>> valuesOfMapJ >>> decodeJson

foreign import quadraticRootsSetImpl :: Json -> Json

quadraticRootsSet:: Quadratic -> Either JsonDecodeError (Set Complex)
quadraticRootsSet = encodeJson >>> quadraticRootsSetImpl >>> decodeJson

newtype MyPair a = MP (Pair a)

unMp::forall a. MyPair a -> Pair a
unMp (MP x) = x

instance decodeJsPair :: DecodeJson a=> DecodeJson (MyPair a) where
  decodeJson json = do
    arr <- decodeJson json
    case arr of
      [i1,i2] -> map MP $ Pair <$> (decodeJson i1) <*> (decodeJson i2)
      _ -> Left $ TypeMismatch "Pair"

quadraticRootsSafe:: Quadratic -> Either JsonDecodeError (Pair Complex)
quadraticRootsSafe = encodeJson >>> quadraticRootsSetImpl >>> decodeJson >>> map unMp

parseAndDecodeArray2D :: String -> Either String (Array (Array Int))
parseAndDecodeArray2D s = jsonParser s >>= decodeJson >>> lmap (const s)

data Tree a
  = Leaf a
  | Branch (Tree a) (Tree a)

derive instance genericTree :: Generic (Tree a) _

instance encodeTree ::EncodeJson a => EncodeJson (Tree a) where
  encodeJson a = genericEncodeJson a

instance decodeTree :: DecodeJson a => DecodeJson (Tree a) where
  decodeJson x = genericDecodeJson x

instance showTree :: Show a=> Show (Tree a) where
  show x = genericShow x

instance eqTree :: Eq a=> Eq (Tree a) where
  eq a b = genericEq a b

data IntOrString
  = IntOrString_Int Int
  | IntOrString_String String

instance iosEncode :: EncodeJson IntOrString where
  encodeJson ( IntOrString_Int i) = encodeJson i
  encodeJson (IntOrString_String s) = encodeJson s

instance iosDecode :: DecodeJson IntOrString where
  decodeJson j =  IntOrString_Int  <$> decodeJson j <|> IntOrString_String <$> decodeJson j

derive instance genericios :: Generic IntOrString _

instance showIos :: Show IntOrString where
  show a = genericShow a

instance eqIos :: Eq IntOrString where
  eq a b = genericEq a b

foreign import toMaybeImpl :: forall a. (a -> Maybe a) -> Maybe a -> Undefined a -> Maybe a 

toMaybe :: forall a. Undefined a -> Maybe a
toMaybe = toMaybeImpl Just Nothing