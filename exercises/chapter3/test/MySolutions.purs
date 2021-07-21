module Test.MySolutions where

import Prelude

import Data.AddressBook (Entry, AddressBook, findEntry)
import Data.List (List, filter, head, nubBy)
import Data.Maybe (Maybe(..))

-- Note to reader: Add your solutions to this file
findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet st = head <<< filter byStreet
  where byStreet e = e.address.street == st

isInBook :: String-> String -> AddressBook-> Boolean
isInBook n s b = case findEntry n s b  of
  Nothing -> false
  _ -> true

removeDuplicates:: List Entry -> List Entry
removeDuplicates l =  nubBy compareEntry l
  where compareEntry e1 e2 = e1.firstName == e2.firstName && e1.lastName == e2.lastName
