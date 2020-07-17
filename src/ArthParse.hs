module ArthParse where

import Types
import Tokens
import Data.List
import Data.Maybe

-- | Takes a string and prints either an arithmetic expression
-- or an error message according to the error in the input string on the screen
parseExpIO :: String -> IO()
parseExpIO str = case parseExp str of
  Left [errStmt,errPos] -> putStr.unlines $ [("Error! " ++ errStmt),errPos]
  Right exp -> putStrLn.show $ exp

-- | Takes a string and returns either an arithmetic expression
-- or an error string according to the error in the input string
parseExp :: String -> Either [String] ArthExp
parseExp str = case mkExp tList of
  Left (error,n) -> Left [errorStmt error ,(showError noSpaceStr n)]
  Right exp      -> Right exp
 where tList = tokenize str
       noSpaceStr = filter (/=' ') str

-- | Takes an error and returns an error message accordingly
errorStmt :: Error -> String
errorStmt error = case error of
  ErrorExp -> " Invalid Expression"
  ParenError -> " Parenthesis Mismatch"
  TokenError -> " Invalid Operator or Character in Arithmetic Expression"
  OprError -> " Invalid arrangement of operators"
  ExpInParenError -> " Invalid Expression in Parenthesis"

-- | Takes an expression string and error position as Int value
-- and returns the same string with error highlighted
showError :: String -> Int -> String
showError str n = unlines $ fmap ("     "++) [str,(spaces ++ "^^^")]
  where spaces = replicate (n) ' '

-- | Takes a token list and returns either an arithmetic expression
-- or an error with its position as a tuple
mkExp :: [Token] -> Either (Error,Int) ArthExp
mkExp tlist = helperME tlist EmptyExp 0

helperME :: [Token]                       -- List of tokens for the expression
           -> ArthExp                     -- Accumulator expression in which elements are
                                          -- to be inserted from token list
           -> Int                         -- Int value representing the position of error
                                          -- (if any) in the input token list
           -> Either (Error,Int) ArthExp  -- Final result - either an arithmetic expression
                                          -- or an error with its position as a tuple
helperME [] exp n = case chkEmpty exp of
  False -> Right exp
  True -> Left (OprError,n)
helperME (x:xs) exp n
  | x == TOpen = insertParenExpInExp
  | otherwise = insertTokenInExp
  where
    insertParenExpInExp = case parExp xs 1 of
      Nothing -> Left (ParenError,n)
      Just ls -> case mkExp ls of
        Left (error,x) -> Left (ExpInParenError,(n+x))
        Right parenExp -> case insertPar parenExp exp of
          Left error -> Left (error,n)
          Right exp -> helperME (drop (succ $ length ls) xs) exp (n+1)
    insertTokenInExp = case insertInExp exp x of
      Left error -> Left (error,n)
      Right exp -> helperME xs exp (n+1)

-- | Takes two expressions and inserts the first expression inside a paraenthesis
-- and then inserts the expression hence formed in the second expression 
insertPar :: ArthExp -> ArthExp -> Either Error ArthExp
insertPar expr EmptyExp = Right (Bracket expr)
insertPar expr (Branch lExp op EmptyExp) = Right (Branch lExp op (Bracket expr))
insertPar expr (Branch lExp op rExp) = Right (Branch lExp op (fromRight $ insertPar expr rExp))
insertPar expr (Bracket exp) = Right (Bracket (fromRight $ insertPar expr exp))
insertPar _ _ = Left ErrorExp

-- | Takes a list of tokens and an Int value which represents the starting of
-- left parenthesis and returns either the token list for the expression
-- inside that parenthesis or Nothing if the corresponding right parenthesis
-- is not found
parExp :: [Token] -> Int -> Maybe [Token]
parExp [] _ = Nothing
parExp (x:xs) n
  |x == TOpen = fmap (x:) $ parExp xs (n+1)
  |x == TClose && n==1 = Just []
  |x == TClose = fmap (x:) $ parExp xs (n-1)
  |otherwise = fmap (x:) $ parExp xs n

-- | Takes an arithmetic expression and a token and inserts its actual value
-- it in the expression according to its type
insertInExp :: ArthExp -> Token -> Either Error ArthExp
insertInExp _ TError = Left TokenError
insertInExp exp tk
  | isTokenInt tk = insInt (tkToInt tk) exp
  | isTokenOp tk  = insOp (tkToOpr tk) exp
  | tk == TOpen = Left ParenError
  | tk == TClose = Left ParenError
  | otherwise = Left TokenError

-- | Takes an Int value and an expression and inserts the value in expression
-- either as a new branch or as second branch under already inserted operator
insInt :: Int -> ArthExp -> Either Error ArthExp
insInt n EmptyExp = Right (Val n)
insInt n (Branch lExp opr EmptyExp) = Right (Branch lExp opr (Val n))
insInt n (Branch lExp opr rExp) =  Right (Branch lExp opr (fromRight $ insInt n rExp))
insInt n _ = Left ErrorExp

-- | Takes an arithmetic operator and an expression and inserts it in the expression
-- as a new branch by comparing the priorities of input operator and operator in the
-- topmost branch
insOp :: Opr -> ArthExp -> Either Error ArthExp
insOp op (Branch lExp opr EmptyExp) = Left OprError
insOp op (Val n) = Right (Branch (Val n) op EmptyExp)
insOp op exp@(Branch lExp opr rExp) = case compare (priority op) (priority opr) of
  GT -> Right (Branch lExp opr (Branch rExp op EmptyExp))
  _ -> Right (Branch exp op EmptyExp)
insOp op (Bracket expr) = Right (Branch (Bracket expr) op EmptyExp)
insOp op _ = Left OprError

-- | Takes a Right value of either type and returns the value inside it
fromRight :: Either a b -> b
fromRight (Right b) = b

-- | Takes an arithmetic expression and checks whether any branch is empty
chkEmpty :: ArthExp -> Bool
chkEmpty EmptyExp = True
chkEmpty (Val n)  = False
chkEmpty (Bracket expr) = chkEmpty expr
chkEmpty (Branch lExp op EmptyExp) = True
chkEmpty (Branch lExp op rExp)
 | chkEmpty lExp == False && chkEmpty rExp == False = False
 | otherwise = True

-- | Function to assign Int values as priority to Arithmetic Operators
-- Operators are inserted in expression according to its priority
-- Operators with higher priority are inserted as lower branches an vice versa
priority :: Opr -> Int
priority Div   = 3
priority Mult  = 2
priority Add   = 1
priority Sub   = 0

-- | Function to convert Int Token into Int Value
tkToInt :: Token -> Int
tkToInt (TInt n) = n

-- | Function to convert Operator Token into Arithmetic Operator
tkToOpr :: Token -> Opr
tkToOpr TAdd  = Add
tkToOpr TSub  = Sub
tkToOpr TMult = Mult
tkToOpr TDiv  = Div
