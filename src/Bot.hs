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

-- = IO actions
-- | handle IOErrors caused by disconnects
disconnectHandler :: IOError -> IO ()
disconnectHandler e | isEOFError e = threadDelay 3000000 >> loop
                    | otherwise = ioError e

-- | wait for a ping, then answer it (required by UnrealIRCd)
initConnection :: Handle -> IO ()
initConnection h = do
    str <- decodeUtf8 <$> B.hGetLine h
    case parseIrc str of
      Just (Message _ (Ping token)) -> send h $ Pong token
      Just msg -> print msg >> initConnection h
      Nothing -> initConnection h

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

-- = Proxy engine for pure message processing
-- | a proxy type to represent intermediate processing states  of a hook result
data CommandProxy
    = SimpleProxy Command
    | ScriptProxy ProcData
    | IgnoreProxy
    deriving (Show, Eq)

type ProcData = (Channel, NickName, Char, Text, [Text])

-- | pure mapping from input to intermediate structures
proxify :: Message -> CommandProxy
proxify (Message _ (Ping token)) = SimpleProxy $ Pong token
proxify (Message _ (Kick channel n _))
    | n == NickName nick = SimpleProxy $ Join channel
    | otherwise = IgnoreProxy
proxify (Message (Just src) (PrivMsg channel t)) = proxifyMsg src channel t
proxify _ = IgnoreProxy

-- | proxify privmsg's
proxifyMsg :: Prefix -> Channel -> Text -> CommandProxy
proxifyMsg (UserPrefix nickName _) channel commandStr
    | Just (prefix, input) <- T.uncons commandStr
    , cmd:args <- T.splitOn " " input
    , prefix `elem` prefixes
    = ScriptProxy (targetChannel, nickName, prefix, cmd, args)
    | otherwise = IgnoreProxy
    where targetChannel
            | nick == getChannel channel = Channel $ getNickName nickName
            | otherwise = channel
proxifyMsg _ _ _ = IgnoreProxy

-- | process proxy objects
interpret :: CommandProxy -> IO [Command]
interpret (SimpleProxy cmd) = return [cmd]
interpret (ScriptProxy pData@(channel, _, _, cmd, _))
    | Just ('c', rest) <- T.uncons cmd
    , rest == T.empty = (:[]) <$> genHelp channel
    | otherwise = runProc pData
interpret IgnoreProxy = return []

genHelp :: Channel -> IO Command
genHelp channel =
    (PrivMsg channel . T.intercalate ", " . map pretty) <$> getScripts
    where permStr True = "[*] "
          permStr False = "[ ] "
          pretty (m, b) = permStr b `T.append` T.takeWhile (/='.') (T.pack m)

runProc :: ProcData -> IO [Command]
runProc pData@(channel, _, _, _, _) = do
    run <- (createProc pData . getLoaded) <$> getScripts
    case run of
      Just process -> do
          print pData
          result <- (formatText . T.pack) <$>
              catchIOError (readCreateProcess process "") handler
          return . map (PrivMsg channel) $ result
      Nothing -> return []
    where handler _ = return "Script crashed, inform an admin!"

-- = Helper functions and tools
-- get all avalable scripts
getScripts :: IO [(FilePath, Bool)]
getScripts = do
    files <- getDirectoryContents "." >>= filterM doesFileExist
    perms <- mapM getPermissions files
    return $ zip files (map executable perms)

-- | get all executable scripts from a list (ie. "loaded" to the bot)
getLoaded :: [(FilePath, Bool)] -> [FilePath]
getLoaded = foldr go []
    where go (p, True) ps = p : ps
          go _ ps = ps

-- | create a CreateProcess struct we need to run a script based on
-- a proxy and executable scripts
createProc :: ProcData -> [FilePath] -> Maybe CreateProcess
createProc ( T.unpack . getChannel -> channel
           , T.unpack . getNickName -> nick
           , prefix , T.unpack -> cmd
           , map T.unpack -> args
           ) scripts =
               addVars <$> (proc <$> (("./" ++) <$> match) <*> pure args)
    where match = fst <$> (uncons $ filter (isValidPrefixOf cmd) scripts)
          addVars p = p {
              env = (Just [("NICKNAME", nick), ("PREFIX", [prefix])]) <>
                  (env p) }

isValidPrefixOf :: Eq a => [a] -> [a] -> Bool
isValidPrefixOf [] _ = False
isValidPrefixOf [c] cs = cs == [c]
isValidPrefixOf cs ds = cs `isPrefixOf` ds

formatText :: Text -> [Text]
formatText = T.lines . T.filter (/='\r')
