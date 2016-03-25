{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage = f . words
  where
    f ("I":x:ys) = LogMessage Info (read x) $ unwords ys
    f ("W":x:ys) = LogMessage Warning (read x) $ unwords ys
    f ("E":x:y:zs) = LogMessage (Error (read x)) (read y) $ unwords zs
    f x = Unknown $ unwords x

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) x = x
insert x Leaf = Node Leaf x Leaf
insert x@(LogMessage _ xt _) (Node left y@(LogMessage _ yt _) right)
  | xt > yt = Node left y (insert x right)
  | xt < yt = Node (insert x left) y right

build :: [LogMessage] -> MessageTree
build = foldl (\x y -> insert y x) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder = f
  where
    f Leaf = []
    f (Node left x right) = (f left) ++ [x] ++ (f right)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map (\(LogMessage _ _ x) -> x) . inOrder . build .  filter severeError

severeError :: LogMessage -> Bool
severeError (LogMessage (Error x) _ _) = x > 50
severeError _ = False
