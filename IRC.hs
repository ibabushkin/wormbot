{-# GeneralizedNewtypeDeriving #-}
module IRC where

import System.IO (Handle, hPutStr)

newtype NickName = NickName { getNickName :: String }
    deriving (Show, Eq)
newtype UserName = UserName { getUserName :: String }
    deriving (Show, Eq)
newtype HostName = HostName { getHostName :: String }
    deriving (Show, Eq)
newtype Token = Token { getToken :: String }
    deriving (Show, Eq)
newtype Channel = Channel { getChannel :: String }
    deriving (Show, Eq)

data Command
    = Ping Token
    | Pong Token
    | Nick NickName
    | User UserName
    | Join Channel
    | PrivMsg Channel String
    deriving (Show, Eq)

type Origin = (NickName, HostName)

data Message = Message (Maybe Origin) Command deriving (Show, Eq)

-- | generate a response to a ping from the server
pingToPong :: Command -> Maybe Command
pingToPong (Ping t) = Just $ Pong t
pingToPong _ = Nothing

{- generate user message 
user :: String -> Message
user n = mkMessage "USER" [n, "0", "*", "worm bot"]

-- format a Message to be send
toIrc :: Command -> String
toIrc (Message _ c args) = c ++ ' ':argString
    where argString = unwords (init args) ++ " :" ++ last args ++ "\r\n"

-- a simple parser to clean up our mess of a bot
parseIrc :: String -> Message
parseIrc (':':s) =
    Message (parseOrigin origin) command args'
    where s' | last s == '\r' = init s
             | otherwise = s
          (tokens, lastArg) = case break (==':') s' of
                                (t, ':':l) -> (t, Just l)
                                (t, "") -> (t, Nothing)
          (origin:command:args) = words tokens
          args' = case lastArg of
                    Just x -> args ++ [x]
                    Nothing -> args
parseIrc s = Message Nothing command args'
    where (tokens, ':':lastArg) = break (==':') s
          (command:args) = words tokens
          args' = args ++ [lastArg]

-- parse the origin of a message
parseOrigin :: String -> Maybe Origin
parseOrigin s
    | '!' `elem` s = Just (NickName nick, HostName host)
    | otherwise = Nothing
    where (nick, host) = break (=='!') s
          -}
