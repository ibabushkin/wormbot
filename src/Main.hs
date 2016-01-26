{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Main where

-- TODO: fix imports so we don't need to put Prelude. before stuff
import Control.Concurrent (threadDelay)
import Control.Monad (forever, filterM)

import qualified Data.ByteString as B
import qualified Data.List as L (uncons)
import Data.Text hiding (foldr1)
import Data.Text.Encoding

import Network

import System.Directory
import System.IO
import System.IO.Error
import System.Process

import Defaults
import Hooks
import IRC

-- | a proxy type to represent intermediate results of a hook result
data CommandProxy
    = SimpleProxy Command
    | ScriptProxy Channel NickName Text [Text]
    | IgnoreProxy

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
proxifyMsg (UserPrefix nick _) channel (uncons -> Just (':', t)) =
    case splitOn " " t of
      cmd:args -> ScriptProxy channel nick cmd args
      _ -> IgnoreProxy
proxifyMsg _ _ _ = IgnoreProxy

-- | handle IOErrors caused by disconnects
disconnectHandler :: IOError -> IO ()
disconnectHandler e | isEOFError e = threadDelay 3000000 >> main
                    | otherwise = ioError e

-- | main: connecting (UnrealIRCd compliant) and listen loop
main :: IO ()
main = (flip catchIOError) disconnectHandler $ do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    send h $ Nick (NickName nick)
    send h $ User (UserName nick) (RealName "bot wormbot of some kind")
    initConnection h
    send h $ PrivMsg (Channel "NickServ") ("IDENTIFY " `append` password)
    mapM_ (send h . Join . Channel) chans
    forever $ process h

-- | send a command through a handle
send :: Handle -> Command -> IO ()
send h c = do
    let r = toIrc c
    B.hPutStr h (encodeUtf8 r)
    Prelude.putStrLn $ "Sent: " ++ show r

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
            Just r -> send h r
            Nothing -> return ()
      Nothing -> return ()

-- | process proxy objects
interpret :: CommandProxy -> IO (Maybe Command)
interpret (SimpleProxy cmd) = return $ Just cmd
interpret (ScriptProxy channel nick cmd args) = return Nothing -- TODO do
interpret IgnoreProxy = return Nothing

-- get all avalable scripts
getScripts :: IO [(FilePath, Bool)]
getScripts = do files <- getDirectoryContents "." >>= filterM doesFileExist
                perms <- mapM getPermissions files
                return $ Prelude.zip files (Prelude.map executable perms)

createProc :: CommandProxy -> [FilePath] -> CreateProcess
createProc = undefined

-- wait for a ping, then answer it (required by UnrealIRCd)
initConnection :: Handle -> IO ()
initConnection h = do
    str <- decodeUtf8 <$> B.hGetLine h
    case parseIrc str of
      Just (Message _ (Ping token)) -> send h $ Pong token
      Just msg -> print msg >> initConnection h
      Nothing -> initConnection h
