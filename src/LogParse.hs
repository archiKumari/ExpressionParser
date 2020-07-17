module LogParse where

import Types
import Tokens
import ArthParse
import Data.List

-- | Takes a string and returns either a logical expression 
-- or a string according to the error in the string
mkLogExp :: String -> Either String LogExp
mkLogExp str = case helperLE str of
  Right ([exp1,exp2],lOpr) -> Right (LExp exp1 lOpr exp2)
  Left errorStr -> Left (head errorStr)

-- | Takes a string and returns either two arithmetic operators 
-- and a logical operator as a tuple or a string according to the
-- error in the input string 
helperLE :: String -> Either [String] ([ArthExp],LOpr)
helperLE ls = case groupExpLopr ls "" of
  [(x:xs),lOpr,(y:ys)] -> case elem lOpr tLOprStr of
    True -> case parseExp (x:xs) of
      Right exp1 -> case parseExp (y:ys) of
        Right exp2 -> Right ([exp1,exp2] ,(strToLOpr lOpr))
        Left str2 -> Left (fmap ("Error in 2nd Expression: " ++) str2)
      Left str1 -> Left (fmap ("Error in 1st Expression: " ++) str1)
    False -> Left ["Error in Arrangement!"]
  _ -> Left ["Error in Logical Expression!"]

-- | Takes a string and returns a list of string in which 
-- the second element is logical operator string with 
-- an arithmetic expression string on the either side.
groupExpLopr :: String -> String -> [String]
groupExpLopr [x] acc = ["",reverse acc,[x]]
groupExpLopr (x:y:ys) acc
 |x=='>'&& y=='='= [reverse acc,[x,y],ys]
 |x=='<'&& y=='='= [reverse acc,[x,y],ys]
 |x=='='&& y=='='= [reverse acc,[x,y],ys]
 |x=='>' = [reverse acc,[x],(y:ys)]
 |x=='<' = [reverse acc,[x],(y:ys)]
 |otherwise = groupExpLopr (y:ys) (x:acc)

