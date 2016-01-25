{-# LANGUAGE OverloadedStrings #-}
module Default where

import Data.Text as T

import Test.QuickCheck
import Test.QuickCheck.Instances

import IRC

illegal :: String
illegal = " \r\n\0"

makeLegal :: Text -> Text
makeLegal = T.filter (`notElem` illegal)

letters :: String
letters = ['a'..'z'] ++ ['A'..'Z']

numbers :: String
numbers = "1234567890"

special :: String
special = "-[]{}\\`^_"

-- = Generators for our datatypes

instance Arbitrary NickName where
    arbitrary = (NickName . pack) <$>
        (listOf1 . elements $ numbers ++ special ++ letters)

instance Arbitrary UserName where
    arbitrary = (UserName . pack) <$>
        (listOf1 . elements $ numbers ++ special ++ letters)

instance Arbitrary RealName where
    arbitrary = (RealName . pack) <$>
        (listOf1 . elements $ numbers ++ special ++ letters)

instance Arbitrary HostName where
    arbitrary = (HostName . pack) <$>
        (listOf1 . elements $ numbers ++ special ++ letters)

instance Arbitrary ServerName where
    arbitrary = (ServerName . pack) <$>
        (listOf1 . elements $ numbers ++ letters)

instance Arbitrary Token where
    arbitrary = (Token . pack) <$>
        (listOf1 . elements $ numbers ++ letters)

instance Arbitrary Channel where
    arbitrary = (Channel . pack) <$>
        ((:) <$> return '#' <*> (listOf1 . elements $ numbers ++ letters))

instance Arbitrary Command where
    arbitrary = oneof
        [ sendableCommands
        , Notice <$> arbitrary <*> arbitrary
        , Ping <$> arbitrary
        ]

sendableCommands :: Gen Command
sendableCommands = oneof
    [ bijectiveCommands
    , Pong <$> arbitrary
    , User <$> arbitrary <*> arbitrary
    ]

bijectiveCommands :: Gen Command
bijectiveCommands = oneof
    [ Join <$> arbitrary
    , Kick <$> arbitrary <*> arbitrary <*> (makeLegal <$> arbitrary)
    , Nick <$> arbitrary
    , PrivMsg <$> arbitrary <*> (makeLegal <$> arbitrary)
    ]

instance Arbitrary Prefix where
    arbitrary = oneof
        [ UserPrefix <$> arbitrary <*> arbitrary
        , ServerPrefix <$> arbitrary
        ]

instance Arbitrary Message where
    arbitrary = Message <$> arbitrary <*> arbitrary
