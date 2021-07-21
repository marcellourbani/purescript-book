module Test.MySolutions where

import Data.Functor
import Data.Traversable
import Prelude

import Control.Apply (lift2)
import Data.AddressBook (Address, PhoneNumber, address, phoneNumber)
import Data.AddressBook.Validation (Errors, matches, validateAddress, validatePhoneNumbers)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Validation.Semigroup (V, invalid)


-- Note to reader: Add your solutions to this file
addMaybe = lift2 (+)
subMaybe = lift2 (-)
mulMaybe = lift2 (*)
divMaybe = lift2 (/)

addApply = lift2 (+)
subApply = lift2 (-)
mulApply = lift2 (*)
divApply = lift2 (/)

combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe (Just x) = map Just x
combineMaybe Nothing  = pure Nothing

phoneNumberRegex :: Regex
phoneNumberRegex = unsafeRegex "^\\d{3}-\\d{3}-\\d{4}$" noFlags

stateRegex ::Regex
stateRegex = unsafeRegex "^[A-Za-z]{2}$" noFlags

nonEmptyRegex :: Regex
nonEmptyRegex = unsafeRegex "\\S" noFlags

validateAddressImproved :: Address -> V Errors Address
validateAddressImproved a =
  address <$> matches "Street" nonEmptyRegex a.street
          <*> matches "City"  nonEmptyRegex  a.city
          <*> matches "State" stateRegex a.state

data Tree a = Leaf | Branch (Tree a) a (Tree a)

derive instance genTree :: Generic (Tree a) _
derive instance eqTree ::Eq a => Eq (Tree a)
instance showTree ::Show a => Show (Tree a) where
  show a = genericShow a

instance funcTree :: Functor Tree where
  map _ Leaf = Leaf
  map f (Branch l x r) = Branch (map f l) (f x) (map f r)

instance foldTree :: Foldable Tree where
  foldr _ x Leaf = x
  foldr f x (Branch l m r) = foldr f (f m (foldr f x r) ) l
  foldl _ x Leaf = x
  foldl f x (Branch l m r) = foldl f (f (foldl f x l) m) r
  foldMap _ Leaf = mempty
  foldMap f (Branch l m r) = foldMap f l <> f m <> foldMap f r

instance travTree :: Traversable Tree where
  traverse _ Leaf = pure Leaf
  traverse f (Branch l m r) = Branch <$> (traverse f l) <*> f m <*> traverse f r
  sequence Leaf = pure Leaf
  sequence (Branch l m r) = Branch <$> (sequence l) <*> m <*> sequence r 

traversePreOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePreOrder _ Leaf = pure Leaf
traversePreOrder f (Branch l m r) = ado
  tm <- f m
  tl <- (traversePreOrder f l)
  tr <- (traversePreOrder f r)
  in Branch tl tm tr

traversePostOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePostOrder _ Leaf = pure Leaf
traversePostOrder f (Branch l m r) = ado
  tl <- (traversePostOrder f l)
  tr <- (traversePostOrder f r)
  tm <- f m
  in Branch tl tm tr

type PersonOA
  = { firstName :: String
    , lastName :: String
    , homeAddress :: Maybe Address
    , phones :: Array PhoneNumber
    }
personOA :: String -> String -> Maybe Address -> Array PhoneNumber -> PersonOA
personOA firstName lastName homeAddress phones = { firstName, lastName, homeAddress, phones }

validatePersonOptionalAddress :: PersonOA -> V Errors PersonOA
validatePersonOptionalAddress a =
  personOA <$> matches "FirstName" nonEmptyRegex a.firstName
          <*> matches "LastName"  nonEmptyRegex  a.lastName
          <*> traverse validateAddress a.homeAddress
          <*> validatePhoneNumbers "Phones" a.phones

sequenceUsingTraverse::forall t a m. Traversable t ⇒ Applicative m ⇒ t (m a) → m (t a)
sequenceUsingTraverse x = traverse identity x

traverseUsingSequence :: forall t a m b. Traversable t => Applicative m => (a -> m b) -> t a -> m (t b)
traverseUsingSequence f x = sequence $ map f x