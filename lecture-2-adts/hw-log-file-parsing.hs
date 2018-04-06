
module NewModule where

import Text.Read

data MessageType = Info
                 | Warning
                 | Error Severity
                 deriving (Show, Eq)

newtype Severity = Severity Int deriving (Show, Eq)

parseWord :: String -> Maybe (String, String)
parseWord "" = Nothing
parseWord input = 
  let
    go word [] = (reverse word, [])
    go word (' ':r) = (reverse word, r)
    go word (c:r) = go (c:word) r
  in  Just (go [] input)

parseInt :: String -> Maybe (Int, String)
parseInt s = fmap convert parseWord s
  where convert :: Maybe (String, String) -> Maybe (Int, String)
        convert (Just (word, r)) = fmap (\i -> (i, r)) (readMaybe word)
        convert Nothing = Nothing

parseSeverity :: String -> Maybe (Severity, String)
parseSeverity = fmap (\(i, s) -> (Severity i, s)) . parseInt

--parseMessageType :: String -> Maybe (MessageType, String)
--parseMessageType ("I":r) = Just (Info, r)
--parseMessageType ("W":r) = Just (Warning, r)
--parseMessageType ("E":r) = parseSeverity

