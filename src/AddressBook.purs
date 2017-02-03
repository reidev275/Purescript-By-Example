module AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, null, nubBy)
import Data.Maybe (Maybe)

type Entry =
	{ firstName :: String
	, lastName  :: String
	, address   :: Address
	}

type Address =
	{ street :: String
	, city   :: String
	, state  :: String
	}

type AddressBook = List Entry

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <>
				  entry.firstName <> ": " <>
				  showAddress entry.address

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <>
				   addr.city <> ", " <>
				   addr.state
	
emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

secret :: String
secret = "Hello"


--head :: forall a. List a -> Maybe a
--filter :: forall a. (a -> Boolean) -> List a -> List a


sameName :: String -> String -> Entry -> Boolean
sameName firstName lastName entry =
	firstName == entry.firstName && lastName == entry.lastName


findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = filter filterEntry >>> head
	where
	 filterEntry :: Entry -> Boolean
	 filterEntry = sameName firstName lastName

findEntryByAddress :: String -> AddressBook -> Maybe Entry
findEntryByAddress address = filter filterEntry >>> head
	where
	 filterEntry :: Entry -> Boolean
	 filterEntry entry = entry.address.street == address

nameExists :: String -> AddressBook -> Boolean
nameExists name = filter (\x -> x.firstName == name) >>> null >>> not

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy dupeCheck
	where
	 dupeCheck :: Entry -> Entry -> Boolean
	 dupeCheck a b = sameName a.firstName a.lastName b



defaultAddress:: Address
defaultAddress = { street: "12405 Hound Ears Point"
			     , city: "Knoxville"
			     , state: "TN"
			     }

defaultEntry :: Entry
defaultEntry = { firstName: "Reid"
			   , lastName: "Evans"
			   , address: defaultAddress
			   }

