{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Bot where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, filterM)

import qualified Data.ByteString as B
import Data.List (isPrefixOf, uncons)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding

import Network

import System.Directory
import System.IO
import System.IO.Error
import System.Process

import Defaults
import IRC

-- | a proxy type to represent intermediate results of a hook result
data CommandProxy
    = SimpleProxy Command
    | ScriptProxy Channel NickName Text [Text]
    | IgnoreProxy
    deriving (Show, Eq)

-- | pure mapping from input to intermediate structures
proxify :: Message -> CommandProxy
proxify (Message (Just src) cmd) =
    case cmd of
      Ping token -> SimpleProxy $ Pong token
      Kick channel n _ | n == NickName nick -> SimpleProxy $ Join channel
                       | otherwise -> IgnoreProxy
      PrivMsg channel t -> proxifyMsg src channel t
      _ -> IgnoreProxy
proxify _ = IgnoreProxy

-- | proxify privmsg's
proxifyMsg :: Prefix -> Channel -> Text -> CommandProxy
proxifyMsg (UserPrefix nickName _) channel (T.uncons -> Just (prefix, t))
    | prefix `elem` prefixes =
        case T.splitOn " " t of
          cmd:args -> ScriptProxy targetChannel nickName cmd args
          _ -> IgnoreProxy
    | otherwise = IgnoreProxy
    where targetChannel
            | nick == getChannel channel = Channel $ getNickName nickName
            | otherwise = channel
proxifyMsg _ _ _ = IgnoreProxy

-- | handle IOErrors caused by disconnects
disconnectHandler :: IOError -> IO ()
disconnectHandler e | isEOFError e = threadDelay 3000000 >> loop
                    | otherwise = ioError e

-- | loop: connecting (UnrealIRCd compliant) and listen loop
loop :: IO ()
loop = (flip catchIOError) disconnectHandler $ do
    dir <- doesDirectoryExist "scripts/"
    createDirectoryIfMissing dir "scripts/"
    setCurrentDirectory "scripts/"
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    send h $ Nick (NickName nick)
    send h $ User (UserName nick) (RealName "bot wormbot of some kind")
    initConnection h
    send h $ PrivMsg (Channel "NickServ") ("IDENTIFY " `T.append` password)
    mapM_ (send h . Join . Channel) chans
    forever $ process h

-- | send a command through a handle
send :: Handle -> Command -> IO ()
send h c = do
    let r = toIrc c
    B.hPutStr h (encodeUtf8 r)
    putStrLn $ "Sent: " ++ show r

-- | process an incoming line from the server:
-- Essentially, this is a hook engine
process :: Handle -> IO ()
process h = do
    line <- decodeUtf8 <$> B.hGetLine h
    case parseIrc line of
      Just msg -> do
          print msg
          res <- interpret (proxify msg)
          case res of
            [] ->  return ()
            rs -> mapM_ (send h) rs
      Nothing -> return ()

-- | process proxy objects
interpret :: CommandProxy -> IO [Command]
interpret (SimpleProxy cmd) = return [cmd]
interpret proxy@(ScriptProxy channel _ cmd _)
    | T.length cmd /= 1 = do
        run <- (createProc proxy . getLoaded) <$> getScripts
        case run of
          Just process -> do
              result <- (formatText . T.pack) <$>
                  catchIOError (readCreateProcess process "") handler
              return . map (PrivMsg channel) $ result
          Nothing -> return []
    | otherwise = return []
    where handler _ = return "Script crashed, inform an admin!"
          nonNewline = (`notElem` ("\r\n" :: String))
interpret IgnoreProxy = return []

-- get all avalable scripts
getScripts :: IO [(FilePath, Bool)]
getScripts = do
    files <- getDirectoryContents "." >>= filterM doesFileExist
    perms <- mapM getPermissions files
    return $ zip files (map executable perms)

-- | get all scripts from a list that are executable (ie. "loaded" to the bot)
getLoaded :: [(FilePath, Bool)] -> [FilePath]
getLoaded = foldr go []
    where go (p, True) ps = p : ps
          go _ ps = ps

-- TODO: we kinda have redundant pattern matches here
-- | create a CreateProcess struct we need to run a script based on
-- a proxy and executable scripts
createProc :: CommandProxy -> [FilePath] -> Maybe CreateProcess
createProc (ScriptProxy
               (T.unpack . getChannel -> channel)
               (T.unpack . getNickName -> nick)
               (T.unpack -> cmd)
               (map T.unpack -> args)
           ) scripts = addVars <$> (
               proc <$> (("./" ++) <$> match) <*> pure args)
    where match = fst <$> (uncons $ filter (cmd `isPrefixOf`) scripts)
          addVars p = p { env = (Just [("NICKNAME", nick)]) <> (env p) }
createProc _ _ = Nothing

-- | wait for a ping, then answer it (required by UnrealIRCd)
initConnection :: Handle -> IO ()
initConnection h = do
    str <- decodeUtf8 <$> B.hGetLine h
    case parseIrc str of
      Just (Message _ (Ping token)) -> send h $ Pong token
      Just msg -> print msg >> initConnection h
      Nothing -> initConnection h

formatText :: Text -> [Text]
formatText = T.lines . T.filter (/='\r')
