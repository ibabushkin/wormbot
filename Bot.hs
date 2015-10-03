import Control.Concurrent (threadDelay)
import Control.Monad (forever, filterM, liftM, when)

import Data.List (isPrefixOf, intercalate)
import Data.Maybe (fromJust, isJust, fromMaybe)

import Network

import System.Directory
import System.IO
import System.IO.Error
import System.Process

import IRC

-- {{{ toplevel constants (do we really need a config file?)

-- the server we connect to
server :: String
server = "irc.evilzone.org"

-- the port we use
port :: Int
port = 6667

-- list of channels to join
chans :: [String]
chans = ["#test", "#bottest", "#Evilzone"]

-- nick to use
botnick :: String
botnick = "wormbot"

-- nickserv password to use
nickservPass :: String
nickservPass = "wormsmakegreatpasswords"

-- prefix for normal commands
commandPrefixes :: [Char]
commandPrefixes = ":<|.\\"

-- prefix for commands that get user's nick as first arg
userPrefix :: Char
userPrefix = '@'
-- }}}

-- little helper
prefixes :: [Char]
prefixes = userPrefix:commandPrefixes

-- main function
main :: IO ()
main = catchIOError main' handler
    where main' = do dir <- doesDirectoryExist "scripts/"
                     createDirectoryIfMissing dir "scripts/"
                     setCurrentDirectory "scripts/"
                     h <- connectTo server (PortNumber (fromIntegral port))
                     hSetBuffering h NoBuffering
                     sendNick h botnick
                     sendUser h botnick
                     initConnection h
                     identify h
                     mapM_ (write h . join) chans
                     listen h
          handler e | isEOFError e = threadDelay 3000000 >> main
                    | otherwise = ioError e

-- the listen "loop"
listen :: Handle -> IO ()
listen h = forever $ do s <- hGetLine h
                        process h (parseIrc s)

-- wait for a ping, then answer it (required by evilzone)
initConnection :: Handle -> IO ()
initConnection h = do s <- hGetLine h
                      let msg = parseIrc s
                      if command msg == "PING"
                         then sendPong h (head $ args msg)
                         else putStrLn s >> initConnection h

-- identify with NickServ
identify :: Handle -> IO ()
identify h = write h msg
    where msg = mkMessage
             "PRIVMSG" ["NickServ", "IDENTIFY " ++ nickservPass]

-- process a Message we get (helper)
process :: Handle -> Message -> IO ()
process h msg = do putStrLn $ ">> " ++ show msg
                   processMessage h msg

-- process a message
processMessage :: Handle -> Message -> IO ()
processMessage h msg
    | command msg == "PING" = sendPong h (head $ args msg)
    | command msg == "PRIVMSG" = processCommand h (fst <$> origin msg) args'
    | command msg == "KICK" = processKick h (args msg)
    | otherwise = return ()
    where args''@(channel:rest) = args msg
          sourceNick = fst $ fromMaybe ("", "") (origin msg)
          args' | channel == botnick = sourceNick:rest
                | otherwise = args''

-- process a comand we got via PRIVMSG
processCommand :: Handle -> Maybe String -> [String] -> IO ()
processCommand h _ [channel, ":c"] =
    do scripts <- getScripts
       perms <- mapM getPermissions scripts
       let modules = map (takeWhile (/='.')) scripts
           modules' = zipWith pretty modules perms
           returnStr = intercalate ", " modules'
       sendPrivmsg h channel returnStr
    where permStr p | executable p = "[*] "
                    | otherwise = "[ ] "
          pretty m p = permStr p ++ m
processCommand h (Just nickName) [channel, p:call]
    | p `elem` prefixes && call' /= [] =
        do result <- evaluateScript command args
           when (result /= []) $
               mapM_ (sendPrivmsg h channel) result
    | otherwise = return ()
    where call' = case words call of
                    [] -> []
                    c@(com:ar) | p == userPrefix -> com:nickName:ar
                               | otherwise -> c
          command:args = call'
processCommand _ _ _ = return ()

-- someone got kicked... make sure that we return if it was us
processKick :: Handle -> [String] -> IO ()
processKick h [channel, nick, _]
    | nick == botnick = sendJoin h channel
    | otherwise = return ()
processKick _ _ = return ()

-- get all avalable scripts
getScripts :: IO [FilePath]
getScripts = getDirectoryContents "." >>= filterM doesFileExist

-- run a script and return it's stdout
evaluateScript :: String -> [String] -> IO [String]
evaluateScript c input
    | c' /= "" = do scripts <- getScripts
                    let possible = filter (c' `isPrefixOf`) scripts
                        process = (proc ("./" ++ command possible) input) { std_out = CreatePipe }
                    (_, out, _, _) <- catchIOError (createProcess process) handler
                    if isJust out
                       then liftM (lines . filter (/='\r')) (hGetContents (fromJust out))
                       else return []
    | otherwise = return []
    where command (c:_) = c
          command _ = ""
          c' = filter (`notElem` "\\/.~") c
          handler e | isPermissionError e = return (undefined, Nothing, undefined, undefined)
                    | otherwise = ioError e
