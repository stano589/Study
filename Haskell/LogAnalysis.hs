module LogAnalysis where
import Log

parseHelper :: [String] -> LogMessage
parseHelper ("E" : rst) = LogMessage (Error (read (head rst))) (read (head (tail rst))) (unwords (tail (tail rst)))
parseHelper ("I" : rst) = LogMessage Info (read (head rst)) (unwords (tail rst))
parseHelper ("W" : rst) = LogMessage Warning (read (head rst)) (unwords (tail rst))
parseHelper rst = Unknown (concat rst)

parseMessage :: String -> LogMessage
parseMessage msg = parseHelper (words msg)

parser :: [String] -> [LogMessage]
parser (x:xs) = [parseMessage x] ++ parser xs
parser [] = []

parse :: String -> [LogMessage]
parse msgs = parser (lines msgs)

insert :: LogMessage -> MessageTree -> MessageTree
insert msg Leaf = Node Leaf msg Leaf
insert (Unknown x) tree = tree
insert (LogMessage msgtype ts msg) (Node leftT (LogMessage inMsgType inTs inMsg) rightT)    
    | inTs == ts = Node leftT (LogMessage msgtype ts msg) rightT
    | inTs < ts = Node leftT (LogMessage inMsgType inTs inMsg) (insert (LogMessage msgtype ts msg) rightT)
    | inTs > ts = Node (insert (LogMessage msgtype ts msg) leftT) (LogMessage inMsgType inTs inMsg) rightT
insert _ _ = Leaf

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftTree msg rightTree)  = inOrder leftTree ++ [msg] ++ inOrder rightTree 

filterMsg :: [LogMessage] -> [String]
filterMsg ((LogMessage (Error x) ts msg):xs) = 
    if x >= 50 then
        msg : filterMsg xs
    else
        filterMsg xs
filterMsg (x:xs) = filterMsg xs
filterMsg [] = []

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong x = filterMsg(inOrder (build x))