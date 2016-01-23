{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)

import qualified Data.ByteString as B
import Data.Text hiding (foldr1)
import Data.Text.Encoding
import Data.Text.IO as TIO

import Network

import System.IO
import System.IO.Error

import Defaults
import Hooks
import IRC

-- | a proxy type to represent intermediate results of a hook result
data CommandProxy
    = Proxy Command
    | IgnoreProxy

-- | pure mapping from input to intermediate structures
proxify :: Message -> CommandProxy
proxify (Message src cmd) =
    case cmd of
      Ping token -> Proxy $ Pong token
      Kick channel n _ | n == NickName nick -> Proxy $ Join channel
                       | otherwise -> IgnoreProxy
      _ -> IgnoreProxy

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
    let r = append (toIrc c) "\r\n"
    TIO.hPutStr h r
    Prelude.putStrLn $ "Sent: " ++ show r

-- | process an incoming line from the server:
-- Essentially, this is a hook engine
process :: Handle -> IO ()
process h = do
    line <- decodeUtf8 <$> B.hGetLine h
    TIO.putStrLn $ "Recieved: " `append` line
    case parseIrc line of
      Just msg -> do
          res <- interpret (proxify msg)
          case res of
            Just r -> send h r
            Nothing -> return ()
      Nothing -> return ()

-- wait for a ping, then answer it (required by UnrealIRCd)
initConnection :: Handle -> IO ()
initConnection h = do
    str <- TIO.hGetLine h
    case parseIrc str of
      Just (Message _ (Ping token)) -> send h $ Pong token
      Just msg -> print msg >> initConnection h
      Nothing -> TIO.putStr "Error parsing: " >> print str >> initConnection h

interpret :: CommandProxy -> IO (Maybe Command)
interpret (Proxy cmd) = return $ Just cmd
interpret IgnoreProxy = return Nothing
