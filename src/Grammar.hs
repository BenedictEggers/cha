{- ISO C99 grammar, represented as Haskell datatypes.
 -
 - Source: http://slps.github.io/zoo/c/iso-9899-tc3.html
 -
 - Also, you'll notice that many of the types have pretty ugly names. I'm not
 - really sure how to get around this--it seems built into the problem of
 - turning the C grammar into semantically-named Haskell types. If you have
 - a way of avoiding this, please contact me immediately via email or
 - carrier pigeon.
 -
 - beggerss@cs.washington.edu
 -}


module Grammar where


import Data.Maybe


{-
 - Here's the type naming scheme:
 -      * Types are given the same names as in the grammars, with some
 -        abbreviations (see below).
 -      * Value constructors are prefixed with the capital letters of the type
 -        they construct, eg an ExtDecl value constructor in the TransUnit type
 -        will be called TUExtDecl.
 -          (i)  In general, value constructors' names will indicate which fields
 -               it expects. However,
 -          (ii) If a type only has one value constructor, it will be named
 -               the same thing as the type (but with a prefix as described above).
 -      * The following substitutions are made in all type names and value
 -        constructors:
 -          - Translation   -> Trans
 -          - External      -> Ext
 -          - Declaration   -> Decl      # "Declarator" will always be written in full
 -          - Function      -> Func
 -          - Definion      -> Def
 -          - Specifier     -> Spec
 -          - Qualifier     -> Qual
 -          - Expression    -> Exp
 -          - Logical       -> Log
 -          - Argument      -> Arg
 -          - Paramater     -> Param
 -          - Constant      -> Const
 -          - Identifier    -> Ident
 -              
 -}



data TransUnit = TUExtDecl ExtDecl
               | TUTransUnitExtDecl TransUnit ExtDecl
               deriving (Show, Eq)

data ExtDecl = EDFuncDef FuncDef
             | EDDecl Decl
             deriving (Show, Eq)

data FuncDef = FDFuncDef DeclSpecs Declarator Maybe DeclList CompoundStatement

data DeclSpecs = DeclSpecsTODO

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

data StructOrUnionSpec = SOUSTODO

data StructOrUnion = Struct
                   | Union
                   deriving (Show, Eq)

data StructDeclList = SDLTODO

data StructDecl = SDTODO

data SpecQualList = SQLTODO

data TypeQual = Const
              | Restrict
              | Volatile
              deriving (Show, Eq)

data StructDeclaratorList = SDLTODOFUCK

data StructDeclarator = TODOTODO

data Declarator = DeclaratorTODO

data Pointer = PointTODO

data TypeQualList = TQLTODO

data DirectDeclarator = DoubleDeeTODO

data AssignmentExpr = AssTODO

data ConditionalExpr = ConditionerTODO

data LogOrExpr = LogarithmTODO

data LogAndExpr = LogarithmToTODO

data InclusiveOrExpr = InclusionTODO

data ExclusiveOrExpr = ExclusionTODO

data AndExpr = AAAAANDTODO

data EqualityExpr = BlackLivesMatterTODO

data RelationalExpr = RelativesTODO

data ShiftExpr = StickyKeysTODO

data AdditiveExpr = AddMoarTODO

data MultiplicativeExpr = TimesMultiplyTODO

data CastExpr = TypeCastTODO

data UnaryExpr = UNITARTHEONETRUEGODTODO

data PostfixExpr = PostalTODO

data PrimaryExpr = PrimesRCoolTODO

data Expr = SomeBSTODO

data ArgExprList = ArgumentTODO

data TypeName = TNTODO

data AbstractDeclarator = AbstractTODO

data DirectAbstractDeclarator = DirectAbstractTODO

data ParamTypeList = PTLRTODO

data ParamList = ListyTODO

data ParamDecl = ParamTODO

data InitializerList = INITIALIZETHESEQUENCETODO

data Designation = DeignTODO

data DesignatorList = DesignatetheTODO

data Designator = DesignatorrrrrTODO

data ConstExpr = ConstantineTODO

data Initializer = InitYooTODO

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

data IdentList = AnotherListTODO

data EnumSpec

data FuncSpec = Inline deriving (Show, Eq)
