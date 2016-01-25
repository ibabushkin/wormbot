{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module IRC where

import Control.Applicative

import Data.Text
import Data.Attoparsec.Text as P

import System.IO (Handle)

-- = newtypes for concern separation
newtype NickName = NickName { getNickName :: Text }
    deriving (Show, Eq)
newtype UserName = UserName { getUserName :: Text }
    deriving (Show, Eq)
newtype RealName = RealName { getRealName :: Text }
    deriving (Show, Eq)
newtype HostName = HostName { getHostName :: Text }
    deriving (Show, Eq)
newtype ServerName = ServerName { getServerName :: Text }
    deriving (Show, Eq)
newtype Token = Token { getToken :: Text }
    deriving (Show, Eq)
newtype Channel = Channel { getChannel :: Text }
    deriving (Show, Eq)

-- | an IRC command of relevance to the bot
data Command
    = Join Channel
    | Kick Channel NickName Text
    | Nick NickName
    | Notice Text Text
    | Ping Token
    | PrivMsg Channel Text
    | Pong Token
    | User UserName RealName
    deriving (Show, Eq)

-- | an IRC message's origin
data Prefix = UserPrefix NickName HostName
            | ServerPrefix ServerName
            deriving (Show, Eq)

-- | an IRC message
data Message = Message (Maybe Prefix) Command deriving (Show, Eq)

-- | generate a representation of an IRC command 
toIrc' :: Command -> Text
toIrc' (Pong token) = "PONG :" `append` getToken token
toIrc' (Nick nick) = "NICK :" `append` getNickName nick 
toIrc' (User uName rName) = "USER " `append`
    getUserName uName `append` " 0 * :" `append` getRealName rName
toIrc' (Join channel) = "JOIN :" `append` getChannel channel
toIrc' (Kick channel nick text) = "KICK " `append` getChannel channel
    `append` " " `append` getNickName nick `append` " :" `append` text
toIrc' (PrivMsg channel text) =
    "PRIVMSG " `append` getChannel channel `append` " :" `append` text
toIrc' _ = Data.Text.empty

toIrc :: Command -> Text
toIrc = (`append` "\r\n") . toIrc'

parseIrc :: Text -> Maybe Message
parseIrc = maybeResult . parse message

-- | parse a message
message :: Parser Message
message = Message <$> prefix <*> command

-- | parse an message's command
-- note that this component recieves lines stripped of the "\n",
-- thus suffixed by "\r" only.
command :: Parser Command
command = choice
    [ Join <$> (string "JOIN" *> (Channel <$> lastArg))
    , Kick <$> (string "KICK" *> (Channel <$> arg)) <*>
        (NickName <$> arg) <*> lastArg
    , Nick <$> (string "NICK" *> (NickName <$> lastArg))
    , Notice <$> (string "NOTICE" *> arg) <*> lastArg
    , Ping <$> (string "PING" *> (Token <$> lastArg))
    , PrivMsg <$> (string "PRIVMSG" *> (Channel <$> arg)) <*> lastArg
    ]
    where noEol = P.takeWhile (/='\r')
          noSpace = P.takeWhile $ (/=' ')
          noColon = P.takeWhile $ (/= ':') 
          lastArg = string " :" *> noEol <* char '\r'
          arg = char ' ' *> noSpace

-- | parse a message prefix
prefix :: Parser (Maybe Prefix)
prefix = Just <$> (userPrefix <|> serverPrefix) <|> return Nothing

-- | parse a user's prefix
userPrefix :: Parser Prefix
userPrefix = UserPrefix <$>
    (char ':' *> (NickName <$> P.takeWhile tokenBoundary <* char '!')) <*>
    (HostName <$> P.takeWhile (/= ' ') <* char ' ')
    where tokenBoundary = (&&) <$> (/= '!') <*> (/= ' ')

-- | parse a server's prefix
serverPrefix :: Parser Prefix
serverPrefix = ServerPrefix <$>
    (char ':' *> (ServerName <$> P.takeWhile (/= ' ')) <* char ' ')
