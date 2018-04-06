
module NewModule where

import Text.Read

data MessageType = Info
                 | Warning
                 | Error Severity
                 deriving (Show, Eq)

newtype Severity = Severity Int deriving (Show, Eq)

newtype Parser a = Parser (String -> Maybe (a, String))

instance Functor Parser where
  --fmap :: (a -> b) -> Parser a -> Parser b
  fmap = undefined

parseWord :: Parser String
parseWord "" = Nothing
parseWord input = 
  let
    go word [] = (reverse word, [])
    go word (' ':r) = (reverse word, r)
    go word (c:r) = go (c:word) r
  in  Just (go [] input)

parseInt :: Parser Int
parseInt s = fmap convert parseWord s
  where convert :: Maybe (String, String) -> Maybe (Int, String)
        convert (Just (word, r)) = fmap (\i -> (i, r)) (readMaybe word)
        convert Nothing = Nothing

parseSeverity :: Parser Severity
parseSeverity = fmap (\(i, s) -> (Severity i, s)) . parseInt

--parseMessageType :: String -> Maybe (MessageType, String)
--parseMessageType ("I":r) = Just (Info, r)
--parseMessageType ("W":r) = Just (Warning, r)
--parseMessageType ("E":r) = parseSeverity

