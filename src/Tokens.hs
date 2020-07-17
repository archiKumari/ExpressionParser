module Tokens where

import Types
import Data.Char
import Data.List

-- | Function to convert the input string into list of tokens
tokenize :: String -> [Token]
tokenize str = combineTokens $ fmap charToToken fStr
  where
    fStr = filter (/=' ') str

-- | Function to combine consecutive Integer tokens into a single Int Tokens
-- and consecutive Logical Operators into a single Logical Operator Token
-- from the Token List of whole Expression
combineTokens :: [Token] -> [Token]
combineTokens [] = []
combineTokens list = concat $ fmap fn2 groupByLOpr
  where
    groupByLOpr = groupBy (\a b -> isTokenLOp a && isTokenLOp b) combinedIntLs
    fn2 list = if (length list > 1) then combineLOprs list else list
    combinedIntLs = concat $ fmap fn1 groupByIntTks
    groupByIntTks = groupBy (\a b -> isTokenInt a && isTokenInt b) list
    fn1 list = if (length list > 1) then combineIntTks list else list

-- | Function to check whether a given token is Integer Token
isTokenInt :: Token -> Bool
isTokenInt (TInt n) = True
isTokenInt _      = False

-- | Function to check whether a given token is Operator Token
isTokenOp :: Token -> Bool
isTokenOp token = elem token tOperators

-- | Function to check whether a given token is Logical Operator Token
isTokenLOp :: Token -> Bool
isTokenLOp token = elem token tLOperators

-- | Function to combine multiple Int Tokens into a single Int Token
-- Multiple digit numbers are parsed as separate Int Tokens,
-- this function combines them into one Int Token
combineIntTks :: [Token] -> [Token]
combineIntTks tList | and (fmap isTokenInt tList) = [TInt (read numStr::Int)]
  where numStr = concat.fmap show $ fmap (\(TInt n) -> n) tList
combineIntTks _ = [TError]

-- | Function to convert multiple Logical Operators into a single Logical Operator
combineLOprs :: [Token] -> [Token]
combineLOprs [TGreater,TEqual] = [TGrtOrEq]
combineLOprs [TLesser ,TEqual] = [TLsrOrEq]
combineLOprs _                 = [TError]

-- | Function to convert Characters into Operators Tokens
charToToken :: Char -> Token
charToToken c | isDigit c = TInt (charToInt c)
charToToken '+' = TAdd
charToToken '-' = TSub
charToToken '*' = TMult
charToToken '/' = TDiv
charToToken '(' = TOpen
charToToken ')' = TClose
charToToken '>' = TGreater
charToToken '<' = TLesser
charToToken '=' = TEqual
charToToken _   = TError

-- | Function to convert Characters into single digit Integer Tokens
charToInt :: Char -> Int
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9

-- | Function to convert String into their respective Logical Operators
strToLOpr :: String -> LOpr
strToLOpr ">" = GreaterThan
strToLOpr "<" = LesserThan
strToLOpr "==" = EqualTo
strToLOpr ">=" = GreaterOrEqual
strToLOpr "<=" = LesserOrEqual
