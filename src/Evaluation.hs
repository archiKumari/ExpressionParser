module Evaluation where

import Types
import Tokens
import ArthParse
import Data.Maybe

-- | Takes an arithmetic expression and returns its value after evaluation
-- if the expression is valid
evalArthExp :: ArthExp -> Maybe Int
evalArthExp (Val n) =  Just n
evalArthExp (Branch (Val a) op (Val b)) = Just $ (opToArthFunc op) a b
evalArthExp (Bracket expr) = evalArthExp expr
evalArthExp exp@(Branch lExp op rExp)
   = evalExp $ Branch (Val (fromJust $ evalArthExp lExp)) op (Val (fromJust $ evalArthExp rExp))
evalExp _ = Nothing

-- | Takes an arithmetic operator and returns a mathematical function
opToArthFunc :: (Integral a) => Opr -> (a->a->a)
opToArthFunc Add  = (+)
opToArthFunc Sub  = (-)
opToArthFunc Mult = (*)
opToArthFunc Div  = div

-- | Takes a logical expression and returns its result after evaluation
evalLogExp :: LogExp -> Bool
evalLogExp (LExp exp1 lOpr exp2) = (opToLogFunc lOpr) exp1 exp2

-- | Takes a logical operator and returns a logical function
opToLogFunc :: (Ord a) => LOpr -> (a->a->Bool)
opToLogFunc GreaterThan    = (>)
opToLogFunc LesserThan     = (<)
opToLogFunc EqualTo        = (==)
opToLogFunc GreaterOrEqual = (>=)
opToLogFunc LesserOrEqual  = (<=)

