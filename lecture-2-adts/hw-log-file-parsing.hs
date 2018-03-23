{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log
import Data.Maybe

parse :: String -> [LogMessage]
parse = (map parseMessage) . lines

parseMessage :: String -> LogMessage
parseMessage s = case words s of
  ("E":sev:time:message) -> LogMessage (Error (read sev)) (read time) (unwords message)
  ("I":time:message) -> LogMessage Info (read time) (unwords message)
  ("W":time:message) -> LogMessage Warning (read time) (unwords message)
  _ -> Unknown s

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert m@(LogMessage _ _ _) Leaf = Node Leaf m Leaf
insert m@(LogMessage _ _ _) (Node l v r)
  | isLater m v = Node l v (insert m r)
  | otherwise = Node (insert m l) v r

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l v r) = (inOrder l) ++ [v] ++ (inOrder r)


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = mapMaybe getErrorMessage

{-
whatWentWrong ((LogMessage (Error sev) _ m):ms)
  | sev >= 50 = m : (whatWentWrong ms)
  | otherwise = whatWentWrong ms
whatWentWrong (m:ms) = whatWentWrong ms
whatWentWrong _ = []
-}

getErrorMessage :: LogMessage -> Maybe String
getErrorMessage (LogMessage (Error sev) _ m)
  | sev >= 50 = Just m
  | otherwise = Nothing
getErrorMessage _ = Nothing

isLater :: LogMessage -> LogMessage -> Bool
isLater (LogMessage _ t1 _) (LogMessage _ t2 _) = t1 > t2
isLater _ _ = True

{-
parseMessage s = case words s of
  ("E":sev:time:message) -> LogMessage (Error (read sev)) (read time) (unwords message)
  ("I":time:message) -> LogMessage Info (read time) (unwords message)
  ("W":time:message) -> LogMessage Warning (read time) (unwords message)
  _ -> Unknown s
-}

{-
messageParts :: String -> Maybe [String]
messageParts s = case length parts of
                    5 -> Just parts
                    4 -> Just parts
                    _ -> Nothing
                 where parts = words s

parseMessageType :: [String] -> (Maybe MessageType, [String])
parseMessageType ("E":x:xs) = (Error <$> (readMaybe x), xs)
parseMessageType ("I":xs) = (Just (Info), xs)
parseMessageType ("W":xs) = (Just (Warning), xs)
parseMessageType xs = (Nothing, xs)

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
                  [(val, "")] -> Just val
                  _           -> Nothing
-}

