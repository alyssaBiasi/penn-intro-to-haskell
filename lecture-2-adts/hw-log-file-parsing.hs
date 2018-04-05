
module NewModule where

data MessageType = Info
                 | Warning
                 | Error Severity
                 deriving (Show, Eq)

type Severity = Int

parseWord :: String -> Maybe (String, String)
parseWord "" = Nothing
parseWord input = 
  let
    go word [] = (reverse word, [])
    go word (' ':r) = (reverse word, r)
    go word (c:r) = go (c:word) r
  in  Just (go [] input)
--parseWord (' ':r) =
--parseWord (c:r) = Just (c : parseWord(r), r)
  

--parseMessageType :: String -> Maybe (MessageType, String)
--parseMessageType ("I":r) = Just (Info, r)
--parseMessageType ("W":r) = Just (Warning, r)
--parseMessageType ("E":r) = parseSeverity


--parseSeverity :: String -> Maybe (Severity, String)
--parseSeverity

