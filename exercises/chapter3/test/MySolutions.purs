module Test.MySolutions where

import Prelude
import Data.Maybe (Maybe)
import Data.List (filter, head, null, nubByEq)
import Data.AddressBook (AddressBook, Entry, findEntry)

-- Note to reader: Add your solutions to this file

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter filterEntryByStreet
  where
    filterEntryByStreet :: Entry -> Boolean
    filterEntryByStreet entry = entry.address.street == street

isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName = not null <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq areEntrySame
  where
    areEntrySame :: Entry -> Entry -> Boolean
    areEntrySame a b = a.firstName == b.firstName && a.lastName == b.lastName