{-# LANGUAGE OverloadedStrings #-}
module Defaults where

import Data.Text (Text)

-- the server we connect to
server :: String
server = "irc.evilzone.org"

-- the port we use
port :: Int
port = 6667

-- nick to use
nick :: Text
nick = "wormbot"

-- nickserv password to use
password :: Text
password = "wormsmakegreatpasswords"

-- list of channels to join
chans :: [Text]
chans = ["#test", "#Evilzone"]

prefixes :: String
prefixes = "<:"
