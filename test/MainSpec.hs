{-# LANGUAGE OverloadedStrings #-}
module MainSpec where

import Data.Text as T

import Test.Hspec
import Test.QuickCheck

import IRC
import Main

-- | main spec
spec :: Spec
spec = describe "Main" $ do
    proxifySpec -- TODO: ignores stuff
    proxifyMsgSpec -- TODO: works properly
    getLoadedSpec -- TODO: same
    createProcSpec -- TODO: never a shell command

proxifySpec :: Spec
proxifySpec = undefined

proxifyMsgSpec :: Spec
proxifyMsgSpec = undefined

getLoadedSpec :: Spec
getLoadedSpec = undefined

creteProcSpec :: Spec
createProcSpec = undefined
