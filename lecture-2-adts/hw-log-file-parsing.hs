{-# LANGUAGE DeriveFunctor #-}
module NewModule where

import qualified Text.Read as TR
import Control.Monad

data MessageType = Info
                 | Warning
                 | Error Severity
                 deriving (Show, Eq)

newtype Severity = Severity Int deriving (Show, Eq, Ord)

newtype Parser a = Parser (String -> Maybe (a, String))

instance Functor Parser where
  --fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser parserF) = Parser $ \s -> fmap mapFirst (parserF s)
    where mapFirst (a, r) = (f a, r)

instance Applicative Parser where
--  pure :: a -> Parser a
  pure a = Parser $ \s -> Just (a, s)
  -- (<*>) :: Parser a -> Parser (a -> b) -> Parser b
  (Parser fa) <*> (Parser fab) = Parser $ \s -> (fa s) >>= convert
    where convert :: (a, String) -> Maybe (b, String)
          convert (a, r) = fmap (\(fb, w) -> (fb a, w)) (fab r)

instance Monad Parser where
-- (>>=) :: Parser a -> (a -> Parse b) -> Parser b
  (>>=) = undefined


parseWord :: Parser String
parseWord =
  let
    go word [] = (reverse word, [])
    go word (' ':r) = (reverse word, r)
    go word (c:r) = go (c:word) r
  in  Parser $ \s -> Just (go [] s)

parseInt :: Parser Int
parseInt = Parser $ \s -> (parse s parseWord) >>= convert
  where convert :: (String, String) -> Maybe (Int, String)
        convert (word, r) = fmap (\i -> (i, r)) (TR.readMaybe word)

parseSeverity :: Parser Severity
parseSeverity = fmap Severity parseInt

parse :: String -> Parser a -> Maybe (a, String)
parse s (Parser f) = f s




--parseMessageType :: String -> Maybe (MessageType, String)
--parseMessageType ("I":r) = Just (Info, r)
--parseMessageType ("W":r) = Just (Warning, r)
--parseMessageType ("E":r) = parseSeverity

