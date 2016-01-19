import Control.Concurrent (threadDelay)
import Control.Monad (forever, filterM, liftM, when)

import Data.List (isPrefixOf, intercalate)
import Data.Maybe (fromMaybe)

import Network

import System.Directory
import System.IO
import System.IO.Error
import System.Process

import IRC
import Hooks (msgHooks, Hook)

-- {{{ toplevel constants (do we really need a config file?)

-- the server we connect to
server :: String
server = "irc.evilzone.org"

-- the port we use
port :: Int
port = 6667

-- list of channels to join
chans :: [String]
chans = ["#test", "#Evilzone"]

-- nick to use
botnick :: NickName
botnick = "wormbot"

-- nickserv password to use
nickservPass :: String
nickservPass = "wormsmakegreatpasswords"

-- prefix for normal commands
commandPrefixes :: String
commandPrefixes = ":<"
-- }}}

-- main function
main :: IO ()
main = catchIOError main' handler
    where main' = do dir <- doesDirectoryExist "scripts/"
                     createDirectoryIfMissing dir "scripts/"
                     setCurrentDirectory "scripts/"
                     h <- connectTo server (PortNumber (fromIntegral port))
                     hSetBuffering h NoBuffering
                     sendNick h botnick -- TODO: BAH
                     sendUser h botnick
                     initConnection h
                     identify h
                     mapM_ (write h . join) chans
                     listen h
          handler e | isEOFError e = threadDelay 3000000 >> main
                    | otherwise = ioError e

-- the listen "loop"
listen :: Handle -> IO ()
listen h = forever $ parseIrc <$> hGetLine h >>= process h 

-- wait for a ping, then answer it (required by UnrealIRCd)
initConnection :: Handle -> IO ()
initConnection h = do
    msg <- parseIrc <$> hGetLine h
    if command msg == "PING"
       then sendPong h (head $ args msg)
       else print msg >> initConnection h

-- identify with NickServ
identify :: Handle -> IO ()
identify h = sendPrivmsg h "NickServ" ("IDENTIFY " ++ nickservPass)

-- process a Message we get (helper)
process :: Handle -> Message -> IO ()
process h msg = putStrLn (">> " ++ show msg) >> processMessage h msg

-- process a message
processMessage :: Handle -> Message -> IO ()
processMessage h msg
    | command msg == "PING" = sendPong h (head $ args msg)
    | command msg == "PRIVMSG" = processCommand h (fst <$> origin msg) args'
    | command msg == "KICK" = processKick h (args msg)
    | otherwise = return ()
    where args''@(channel:rest) = args msg
          sourceNick = fst $ fromMaybe ("", "") (origin msg)
          -- not sure whether I should remove the fromMaybe
          args' | channel == botnick = sourceNick:rest
                | otherwise = args''

-- process a comand we got via PRIVMSG
processCommand :: Handle -> Maybe NickName -> [String] -> IO ()
processCommand h _ [channel, ":c"] =
    (intercalate ", " . map pretty) <$> getScripts >>= sendPrivmsg h channel
    where permStr True = "[*] "
          permStr False = "[ ] "
          pretty (m, b) = permStr b ++ takeWhile (/='.') m
processCommand h (Just nickName) [channel, p:call]
    | p `elem` commandPrefixes && call' /= [] =
        do result <- evaluateScript nickName [p] command args
           when (result /= []) $ mapM_ (sendPrivmsg h channel) result
    | otherwise = mapM_ (>>= mapM (sendPrivmsg h channel)) $
        runHooks msgHooks call'
    where call' = words call
          command:args = call'
processCommand _ _ _ = return ()

-- run a specified list of hooks
runHooks :: [Hook] -> [String] -> [IO [String]]
runHooks hs wordList = map (\x -> x wordList) hs

-- someone got kicked... make sure that we return if it was us
processKick :: Handle -> [String] -> IO ()
processKick h [channel, nick, _]
    | nick == botnick = sendJoin h channel
    | otherwise = return ()
processKick _ _ = return ()

-- get all avalable scripts
getScripts :: IO [(FilePath, Bool)]
getScripts = do
    files <- getDirectoryContents "." >>= filterM doesFileExist
    perms <- mapM getPermissions files
    return $ zip files (map executable perms)

-- run a script and return it's stdout, the environment is set to the nick in NICKNAME
evaluateScript :: NickName -> String -> String -> [String] -> IO [String]
evaluateScript nickName prefix c input
    | c' /= "" = do
        scripts <- getScripts
        let possible = map fst $ filter check scripts
            process = proc ("./" ++ command possible) input
        putStrLn $ "Possible scripts: " ++ show possible
        if command possible /= ""
           then liftM (lines . filter (/='\r')) $
               catchIOError (readCreateProcess
                   process { env = addStuffToEnv (env process) } "") handler
           else return []
    | otherwise = return []
    where check (s,p) =  c' `isPrefixOf` s && p
          command [c] = c
          command _ = ""
          c' = filter (`notElem` "\\/.~") c
          addStuffToEnv = mappend (Just [("NICKNAME", nickName), ("PREFIX", prefix)])
          handler _ = return "Script crashed, inform an admin!"
