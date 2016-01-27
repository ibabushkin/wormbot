{-# LANGUAGE OverloadedStrings, TupleSections #-}
module BotSpec where

import qualified Data.Text as T

import System.Process

import Test.Hspec
import Test.QuickCheck

import IRC
import Bot

import Default

-- | main spec
spec :: Spec
spec = do
    proxifySpec
    proxifyMsgSpec
    getLoadedSpec
    createProcSpec

proxifySpec :: Spec
proxifySpec = describe "proxify" $ do
    it "ignores messages without a prefix" $ property $
        \cmd -> proxify (Message Nothing cmd) == IgnoreProxy
    it "handles pings correctly" $ property $
        forAll arbitraryPing $
        \msg@(Message _ (Ping token)) ->
            proxify msg == SimpleProxy (Pong token)

proxifyMsgSpec :: Spec
proxifyMsgSpec = describe "proxifyMsg" $ do
    it "gives ScriptProxies and IgnoreProxies" $ property $
        \pr ch text -> case proxifyMsg pr ch text of
                         IgnoreProxy -> True
                         ScriptProxy _ _ _ _ -> True
                         _ -> False

getLoadedSpec :: Spec
getLoadedSpec = describe "getLoaded" $ do
    it "honours length invariants" $ property $
        \input -> length (getLoaded input) <= length input
    it "works for corner cases: all" $ property $
        \input -> (length . getLoaded $ map (,True) input) == length input
    it "works for corner cases: none" $ property $
        \input -> (length . getLoaded $ map (,False) input) == 0

createProcSpec :: Spec
createProcSpec = describe "createProc" $ do
    it "never returns shell commands" $ property $
        forAll arbitraryScriptProxy $
        \pr ps -> case cmdspec <$> createProc pr ps of
                    Just (RawCommand _ _) -> True
                    Nothing -> True
                    _ -> False
