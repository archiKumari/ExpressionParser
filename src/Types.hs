module Types where

-- | Data type for representing an Arithmetic Expression
data ArthExp = EmptyExp
               | Val Int
               | Branch ArthExp Opr ArthExp
               | Bracket ArthExp
  deriving (Show,Ord,Eq)

-- | Data type for representing different Arithmetic Operators
data Opr = Add
           | Sub
           | Mult
           | Div
  deriving (Show,Ord,Eq)

data Error = ErrorExp
             | ParenError
             | OprError
             | TokenError
             | ExpInParenError
  deriving (Show,Eq)

-- | Data type for representing a Logical Expression
data LogExp = LExp ArthExp LOpr ArthExp
              | LError
  deriving (Show,Eq)

-- | Data type for representing different Logical Operators
data LOpr = GreaterThan
            | LesserThan
            | EqualTo
            | GreaterOrEqual
            | LesserOrEqual
            | NotEqual
  deriving (Show,Eq)

-- | Data type for storing operators and integer values as a single entity Token
data Token = TInt Int
           | TAdd
           | TSub
           | TMult
           | TDiv
           | TOpen
           | TClose
           | TGreater
           | TLesser
           | TEqual
           | TGrtOrEq
           | TLsrOrEq
           | TError
  deriving (Show,Eq)

-- | List of ArithMetic Operator Tokens
tOperators = [TAdd,TSub,TMult,TDiv]

-- | List of Logical Operator Tokens
tLOperators = [TGreater,TLesser,TEqual,TGrtOrEq,TLsrOrEq]
tLOprStr = [">","<","==",">=","<="]

