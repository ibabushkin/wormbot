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
newtype Token = Token { getToken :: Text }
    deriving (Show, Eq)
newtype Channel = Channel { getChannel :: Text }
    deriving (Show, Eq)

-- | an IRC command of relevance to the bot
data Command
    = Ping Token
    | Pong Token
    | Nick NickName
    | User UserName RealName
    | Join Channel
    | Kick Channel NickName Text
    | PrivMsg Channel Text
    deriving (Show, Eq)

-- | an IRC message's origin
type Origin = (NickName, HostName)

-- | an IRC message
data Message = Message (Maybe Origin) Command deriving (Show, Eq)

-- | generate a representation of an IRC command 
toIrc :: Command -> Text
toIrc (Ping token) = "PING :" `append` getToken token
toIrc (Pong token) = "PONG :" `append` getToken token
toIrc (Nick nick) = "NICK :" `append` getNickName nick 
toIrc (User uName rName) = "USER " `append`
    getUserName uName `append` " 0 *:" `append` getRealName rName
toIrc (Join channel) = "JOIN :" `append` getChannel channel
toIrc (Kick channel nick text) = "KICK :" `append` getChannel channel
    `append` " " `append` getNickName nick `append` ":" `append` text
toIrc (PrivMsg channel text) =
    "PRIVMSG " `append` getChannel channel `append` " :" `append` text

parseIrc :: Text -> Maybe Message
parseIrc = maybeResult . parse message

-- | parse a message
message :: Parser Message
message = Message <$> origin <*> command

-- | parse an message's command
-- note that this component recieves lines stripped of the "\n",
-- thus suffixed by "\r" only.
command :: Parser Command
command = choice
    [ Ping <$> (string "PING" *> (Token <$> lastArg))
    , Nick <$> (string "NICK" *> (NickName <$> lastArg))
    , Join <$> (string "JOIN" *> (Channel <$> lastArg))
    , Kick <$> (string "KICK" *> (Channel <$> arg)) <*>
        (NickName <$> arg) <*> lastArg
    , PrivMsg <$> (string "PRIVMSG" *> (Channel <$> arg)) <*> lastArg
    ]
    where noEol = P.takeWhile (/='\r')
          noSpace = P.takeWhile $ (&&) <$> (/=' ') <*> (/='\t')
          noColon = P.takeWhile $ (/= ':') 
          lastArg = string " :" *> noEol <* char '\r'
          arg = char ' ' *> noSpace

-- | parse a message's origin
origin :: Parser (Maybe Origin)
origin = Just <$> ( (,) <$>
    (char ':' *> (NickName <$> P.takeWhile (/= '!') <* char '!')) <*>
    (HostName <$> P.takeWhile (/= ' ') <* char ' ')) <|> return Nothing
