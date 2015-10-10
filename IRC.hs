module IRC where

import System.IO (Handle, hPutStr)

type Command = String

type Origin = (String, String)

data Message = Message { origin :: Maybe Origin
                       , command :: Command
                       , args :: [String]
                       } deriving (Show, Eq)

-- generate a message to be send
mkMessage :: Command -> [String] -> Message
mkMessage = Message Nothing

-- write something to the socket in appropriate format
write :: Handle -> Message -> IO ()
write h msg = do let msgString = toIrc msg
                 hPutStr h msgString
                 putStr msgString

-- generate nick message
nick :: String -> Message
nick n = mkMessage "NICK" [n]

-- send a nick message
sendNick :: Handle -> String -> IO ()
sendNick h = write h . nick

-- generate user message 
user :: String -> Message
user n = mkMessage "USER" [n, "0", "*", "worm bot"]

-- send user message
sendUser :: Handle -> String -> IO ()
sendUser h = write h . user

-- generate join message
join :: String -> Message
join c = mkMessage "JOIN" [c]

-- send join message
sendJoin :: Handle -> String -> IO ()
sendJoin h = write h . join

-- generate pong message
pong :: String -> Message
pong t = mkMessage "PONG" [t]

-- send pong message
sendPong :: Handle -> String -> IO ()
sendPong h = write h . pong 

-- generate private message
privmsg :: String -> String -> Message
privmsg c s = mkMessage "PRIVMSG" [c, s]

-- send private message
sendPrivmsg :: Handle -> String -> String -> IO ()
sendPrivmsg h c s = write h (privmsg c s)

-- format a Message to be send
toIrc :: Message -> String
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
    | '!' `elem` s = Just $ break (=='!') s
    | otherwise = Nothing
