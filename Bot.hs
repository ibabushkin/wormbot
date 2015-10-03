import Control.Concurrent (threadDelay)
import Control.Monad (forever, filterM)

import Data.List (isPrefixOf, intercalate)
import Data.Maybe (fromJust, isJust, fromMaybe)

import Network

import System.Directory
import System.IO
import System.IO.Error
import System.Process

import IRC

-- toplevel constants (do we really need a config file?)
server = "irc.evilzone.org"
port = 6667
chans = ["#test", "#bottest", "#Evilzone"]
botnick = "wormbot"
botAdmins = ["thewormkill"]
nickservPass = "wormsmakegreatpasswords"

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
                     sequence_ $ map (write h) (map join chans)
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
    | command msg == "PRIVMSG" = processCommand h args'
    | command msg == "KICK" = processKick h (args msg)
    | otherwise = return ()
    where args''@(channel:rest) = args msg
          sourceNick = fst $ fromMaybe ("", "") (origin msg)
          args' | channel == botnick = sourceNick:rest
                | otherwise = args''

-- process a comand we got via PRIVMSG
processCommand :: Handle -> [String] -> IO ()
processCommand h (channel:":c":[]) =
    do scripts <- getScripts
       perms <- sequence $ map getPermissions scripts
       let modules = map (fst . break (=='.')) scripts
           modules' = map pretty $ zip modules perms
           returnStr = intercalate ", " modules'
       sendPrivmsg h channel returnStr
    where permStr p | executable p = "[*]"
                    | otherwise = "[ ]"
          pretty (m, p) = permStr p ++ " " ++ m
processCommand h (channel:(':':call):[])
    | call /= [] = do result <- evaluateScript command args
                      if result /= []
                         then mapM_ (sendPrivmsg h channel) result
                         else return ()
    | otherwise = return ()
    where (command:args) = words call
processCommand _ _ = return ()

-- someone got kicked... make sure that we return if it was us
processKick :: Handle -> [String] -> IO ()
processKick h (channel:nick:_:[])
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
                       then hGetContents (fromJust out) >>= return . lines . filter (/='\r')
                       else return []
    | otherwise = return []
    where command (c:_) = c
          command _ = ""
          c' = filter (`notElem` "\\/.~") c
          handler e | isPermissionError e = return (undefined, Nothing, undefined, undefined)
                    | otherwise = ioError e
