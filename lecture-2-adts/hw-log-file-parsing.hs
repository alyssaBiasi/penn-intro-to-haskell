{-# LANGUAGE DeriveFunctor #-}
module NewModule where

import qualified Text.Read as TR
import Control.Monad
import Data.Maybe

data MessageType = Info
                 | Warning
                 | Error Severity
                 deriving (Show, Eq)

type TimeStamp = Int
data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
                deriving (Show, Eq)

newtype Severity = Severity Int deriving (Show, Eq, Ord)

newtype Parser a = Parser {
  parse :: String -> Maybe (a, String)
}

instance Functor Parser where
  --fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser parserF) = Parser $ \s -> fmap mapFirst (parserF s)
    where mapFirst (a, r) = (f a, r)

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser fab) <*> (Parser fa) = Parser $ \s -> ((fa s) >>= convert)
    where convert (a, r) = fmap (\(fb, w) -> (fb a, w)) (fab r)

instance Monad Parser where
-- (>>=) :: Parser a -> (a -> Parse b) -> Parser b
  (Parser fa) >>= fab = Parser $ \s1 ->
    (fa s1) >>= \(a, s2) -> parse (fab a) s2

----------------------------------------------------------

parseWord :: Parser String
parseWord =
  let
    go word [] = (reverse word, [])
    go word (' ':r) = (reverse word, r)
    go word (c:r) = go (c:word) r
  in  Parser $ \s -> Just (go [] s)

--parseInt :: Parser Int
--parseInt = Parser $ \s -> (parse parseWord s) >>= convert
--  where convert :: (String, String) -> Maybe (Int, String)
--        convert (word, r) = fmap (\i -> (i, r)) (TR.readMaybe word)

parseInt :: Parser Int
parseInt = do
  word <- parseWord
  maybeToParser $ TR.readMaybe word

maybeToParser :: Maybe a -> Parser a
maybeToParser = maybe failedParser pure

failedParser :: Parser a
failedParser = Parser $ \_ -> Nothing

recoveringParser :: b -> Parser b -> String -> b
recoveringParser defaultValue (Parser fb) = \s -> fst $ fromMaybe (defaultValue, s) (fb s)


--parseN :: Int -> Parser a -> Parser [a]

-----

(|||) :: Parser a -> Parser a -> Parser a
(|||) (Parser p1) (Parser p2) = Parser $ \s -> maybe (p2 s) Just (p1 s)

(***) :: Semigroup a => Parser a -> Parser a -> Parser a
(***) (Parser p1) (Parser p2) = Parser $ \s -> (p1 s) (p2 2)

instance Monoid (Parser a) where
  mappend = (|||)
  mempty = failedParser


readLogMessage :: String -> LogMessage
readLogMessage s = recoveringParser (Unknown s) parseValidLogMessage s

parseValidLogMessage :: Parser LogMessage
parseValidLogMessage = do
  messageType <- parseMessageType
  timeStamp <- parseTimeStamp
  message <- parseRemaining
  pure $ LogMessage messageType timeStamp message

parseMessageType :: Parser MessageType
parseMessageType = undefined

parseTimeStamp :: Parser TimeStamp
parseTimeStamp = undefined

parseRemaining :: Parser String
parseRemaining = undefined

parseSeverity :: Parser Severity
parseSeverity = fmap Severity parseInt

newParser = do
  w <- parseInt
  v <- parseInt
  return $ w + v
--newParser = parseInt >>= \w -> parseInt >>= \v -> pure (w + v)




--parseMessageType :: String -> Maybe (MessageType, String)
--parseMessageType ("I":r) = Just (Info, r)
--parseMessageType ("W":r) = Just (Warning, r)
--parseMessageType ("E":r) = parseSeverity

{--
Playground code:
  maybe 0 (+1) (Just 3)

  --parse parseLogMessage "E 20 765 everything is broken"

  --parse parseLogMessage "I 877 working working"

  --parse parseLogMessage "W 875 slowly breaking"

  readLogMessage "not a log message"

  recoveringParser 0 parseInt "9"

  --parse ( <|> pure 3 <|> parseInt) "87s"
  --parseWarning <|> parseError <|> parseInfo




  -- commutative
  -- associative

  parseInt ||| failedParser

  parse (parseInt *** parseInt) "5" -- 10
--}
