{-# LANGUAGE DeriveFunctor #-}
module NewModule where

import qualified Text.Read as TR
import Control.Monad

data MessageType = Info
                 | Warning
                 | Error Severity
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


parseWord :: Parser String
parseWord =
  let
    go word [] = (reverse word, [])
    go word (' ':r) = (reverse word, r)
    go word (c:r) = go (c:word) r
  in  Parser $ \s -> Just (go [] s)

parseInt :: Parser Int
parseInt = Parser $ \s -> (parse parseWord s) >>= convert
  where convert :: (String, String) -> Maybe (Int, String)
        convert (word, r) = fmap (\i -> (i, r)) (TR.readMaybe word)

parseSeverity :: Parser Severity
parseSeverity = fmap Severity parseInt


parseInt2 :: Parser Int
parseInt2 = do
  word <- parseWord
  pure TR.readMaybe word


newParser = do
  w <- parseInt
  v <- parseInt
  return $ w + v
--newParser = parseInt >>= \w -> parseInt >>= \v -> pure (w + v)



--parseMessageType :: String -> Maybe (MessageType, String)
--parseMessageType ("I":r) = Just (Info, r)
--parseMessageType ("W":r) = Just (Warning, r)
--parseMessageType ("E":r) = parseSeverity

