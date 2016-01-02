module Hooks where

--import Network.URL

type Hook = [String] -> IO [String]

msgHooks :: [Hook]
msgHooks = []

{-urlHook :: Hook
urlHook [url] = case importURL of
                  Just u -> return []
                  Nothing -> return []
urlHook _ = return []-}
