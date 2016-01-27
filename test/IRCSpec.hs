{-# LANGUAGE OverloadedStrings #-}
module IRCSpec where

import Data.Attoparsec.Text (parseOnly)
import Data.Text as T

import Test.Hspec
import Test.QuickCheck

import IRC

import Default

-- | main spec
spec :: Spec
spec = describe "IRC" $ toIrcSpec

-- = specs

toIrcSpec :: Spec
toIrcSpec = describe "toIrc" $ do
    it "adds correct newlines" $ property $
        \c -> let r = toIrc c
               in T.last r == '\n' && T.last (T.init r) == '\r'
    it "contains a colon before the last argument" $ property $
        forAll sendableCommands $
        (/= 1) . Prelude.length . splitOn " :" . toIrc
    it "is well-behaved towards our parser" $ property $
        forAll bijectiveCommands $
        (==) <$> parseOnly command . T.init . toIrc <*> Right
