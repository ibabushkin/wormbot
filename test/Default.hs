{-# LANGUAGE OverloadedStrings #-}
module Default where

import Data.Text as T

import Test.QuickCheck
import Test.QuickCheck.Instances

import IRC

illegal :: String
illegal = " \r\n\0"
-- = Generators for our datatypes

instance Arbitrary NickName where
    arbitrary = (NickName . pack . Prelude.filter (`notElem` illegal)) <$>
        suchThat arbitrary (not . Prelude.null)

instance Arbitrary UserName where
    arbitrary = (UserName . pack . Prelude.filter (`notElem` illegal)) <$>
        suchThat arbitrary (not . Prelude.null)


instance Arbitrary RealName where
    arbitrary = (RealName . pack) <$>
        suchThat arbitrary (not . Prelude.null)

instance Arbitrary HostName where
    arbitrary = (HostName . pack . Prelude.filter (`notElem` illegal)) <$>
        suchThat arbitrary (not . Prelude.null)

instance Arbitrary ServerName where
    arbitrary = (ServerName . pack . Prelude.filter (`notElem` illegal)) <$>
        suchThat arbitrary (not . Prelude.null)

instance Arbitrary Token where
    arbitrary = (Token . pack . Prelude.filter (`notElem` illegal)) <$>
        suchThat arbitrary (not . Prelude.null)

instance Arbitrary Channel where
    arbitrary = (Channel . pack . Prelude.filter (`notElem` illegal)) <$>
        suchThat arbitrary (not . Prelude.null)

instance Arbitrary Command where
    arbitrary = oneof
        [ sendableCommands
        , Notice <$> arbitrary <*> arbitrary
        , Ping <$> arbitrary
        ]

sendableCommands :: Gen Command
sendableCommands = oneof
    [ Join <$> arbitrary
    , Kick <$> arbitrary <*> arbitrary <*> arbitrary
    , Nick <$> arbitrary
    , PrivMsg <$> arbitrary <*> arbitrary
    , Pong <$> arbitrary
    , User <$> arbitrary <*> arbitrary
    ]

instance Arbitrary Prefix where
    arbitrary = oneof
        [ UserPrefix <$> arbitrary <*> arbitrary
        , ServerPrefix <$> arbitrary
        ]

instance Arbitrary Message where
    arbitrary = Message <$> arbitrary <*> arbitrary
