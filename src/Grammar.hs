{- ISO C99 grammar, represented as Haskell datatypes.
 -
 - Source: http://slps.github.io/zoo/c/iso-9899-tc3.html
 -}

module Grammar where

data StorageClassSpec = Typedef
                      | Extern
                      | Static
                      | Auto
                      | Register
                      deriving (Show, Eq)

data TypeSpec = Void
              | Char
              | Short
              | Int
              | Long
              | Float
              | Double
              | Signed
              | Unsigned
              | Bool
              | Complex
              deriving (Show, Eq)
              -- TODO

data StructOrUnion = Struct
                   | Union
                   deriving (Show, Eq)

data TypeQual = Const
              | Restrict
              | Volatile
              deriving (Show, Eq)

data UnaryOp = Ampersand
             | Star
             | Plus
             | Minus
             | Tilde
             | Bang
             deriving (Show, Eq)

data AssignOp = Equals
              | TimesEquals
              | DivideEquals
              | ModEquals
              | PlusEquals
              | MinusEquals
              | LeftShiftEquals
              | RightShiftEquals
              | BitAndEquals
              | BitXorEquals
              | BitOrEquals
              deriving (Show, Eq)

data FuncSpec = Inline deriving (Show, Eq)
