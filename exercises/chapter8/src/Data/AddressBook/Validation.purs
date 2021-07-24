module Data.AddressBook.Validation where

import Prelude

import Data.AddressBook (Address, Person, PhoneNumber, PhoneType(..), address, person, phoneNumber)
import Data.Either (Either)
import Data.String (length)
import Data.String.Regex (Regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse)
import Data.Validation.Semigroup (V, invalid, toEither)

data Field = FirstNameField
           | LastNameField
           | StreetField
           | CityField
           | StateField
           | PhoneField PhoneType

derive instance eqField :: Eq Field

data ValidationError = ValidationError String Field

fieldLabel :: Field -> String
fieldLabel f = case f of
                 FirstNameField -> "First Name"
                 LastNameField  -> "Last Name"  
                 StreetField    -> "Street"
                 CityField      -> "City"
                 StateField     -> "State"
                 PhoneField p   -> "Phone " <> case p of
                                           HomePhone -> "home"
                                           WorkPhone -> "work"
                                           CellPhone -> "mobile"
                                           OtherPhone -> "other" 

type Errors = Array ValidationError

nonEmpty :: Field -> String -> V Errors String
nonEmpty field ""     = invalid [ ValidationError ("Field '" <> fieldLabel field <> "' cannot be empty") field ]
nonEmpty _     value  = pure value

validatePhoneNumbers :: Array PhoneNumber -> V Errors (Array PhoneNumber)
validatePhoneNumbers  []      =
  invalid [ ValidationError "Field Phone Number must contain at least one value" $ PhoneField CellPhone ]
validatePhoneNumbers phones  =
  traverse validatePhoneNumber phones

lengthIs :: Field -> Int -> String -> V Errors String
lengthIs field len value | length value /= len =
  invalid [ ValidationError ("Field '" <> fieldLabel field <> "' must have length " <> show len) field ] 
lengthIs _     _   value = pure value

phoneNumberRegex :: Regex
phoneNumberRegex = unsafeRegex "^\\d{3}-\\d{3}-\\d{4}$" noFlags

matches :: Field -> Regex -> String -> V Errors String
matches _     regex value | test regex value 
                          = pure value
matches field _     _     = invalid [ ValidationError ("Field '" <> fieldLabel field <> "' did not match the required format") field ]

validateAddress :: Address -> V Errors Address
validateAddress a =
  address <$> nonEmpty StreetField  a.street
          <*> nonEmpty CityField    a.city
          <*> lengthIs StateField 2 a.state

validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber pn =
  phoneNumber <$> pure pn."type"
              <*> matches (PhoneField pn.type) phoneNumberRegex pn.number

validatePerson :: Person -> V Errors Person
validatePerson p =
  person <$> nonEmpty FirstNameField p.firstName
         <*> nonEmpty LastNameField p.lastName
         <*> validateAddress p.homeAddress
         <*> validatePhoneNumbers p.phones

validatePerson' :: Person -> Either Errors Person
validatePerson' p = toEither $ validatePerson p
