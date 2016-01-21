{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IRC where

import Control.Applicative

import Data.Text
import Data.Attoparsec.Text as P

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

-- | generate a response to a ping from the server
pingToPong :: Command -> Maybe Command
pingToPong (Ping t) = Just $ Pong t
pingToPong _ = Nothing

-- | generate a representation of an IRC command 
toIrc :: Command -> Text
toIrc (Ping token) = "PING :" `append` getToken token
toIrc (Pong token) = "PONG :" `append` getToken token
toIrc (Nick nick) = "NICK :"
toIrc (User uName rName) = "USER " `append`
    getUserName uName `append` " 0 *:" `append` getRealName rName
toIrc (Join channel) = "JOIN :" `append` getChannel channel
toIrc (Kick channel nick text) = "KICK :" `append` getChannel channel
    `append` " " `append` getNickName nick `append` ":" `append` text
toIrc (PrivMsg channel text) =
    "PRIVMSG " `append` getChannel channel `append` text

-- | parse a message
message :: Parser Message
message = Message <$> origin <*> command

-- parse an message's command
command :: Parser Command
command = choice
    [ Ping <$> (string "PING " *> (Token <$> lastArg))
    , Nick <$> (string "NICK " *> (NickName <$> lastArg))
    , Join <$> (string "JOIN " *> (Channel <$> lastArg))
    , Kick <$> (string "KICK " *> (Channel <$> arg)) <*>
        (NickName <$> nTLArg) <*> lastArg
    , PrivMsg <$> (string "PRIVMSG " *> (Channel <$> nTLArg)) <*> lastArg
    ]
    where noEol = P.takeWhile $ (&&) <$> (/='\r') <*> (/='\n')
          noSpace = P.takeWhile $ (&&) <$> (/=' ') <*> (/='\t')
          noColon = P.takeWhile $ (/= ':') 
          lastArg = noEol <* string "\r\n"
          nTLArg = noColon <* char ':'
          arg = noSpace <* (char ' ' <|> char '\t')

-- | parse a message's origin
origin :: Parser (Maybe Origin)
origin = Just <$> ( (,) <$>
    (char ':' *> (NickName <$> P.takeWhile (/= '!') <* char '!')) <*>
    (HostName <$> P.takeWhile (notInClass " ") <* char ' ')) <|> return Nothing
