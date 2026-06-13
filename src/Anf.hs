
module Anf (
  Program(..),
  Function(..),
  ArgumentDef(..),
  Lambda(..),
  Expr(..),
  Decl(..),
  ConvOp(..),
  Select(..),
  Icmp(..),
  Flow(..),
  BinOp(..),
  Call(..),
  IfThenElse(..),
  Value(..),
) where

newtype Program = Program [Function] deriving (Show, Eq)

-- | A whole LLVM function. The @[String]@ are the Haskell types of the
-- arguments and the trailing 'String' is the return type; together they form
-- the emitted top-level signature, which anchors bit-width inference.
data Function =
  Function String [ArgumentDef] [String] String Lambda Call
  deriving (Show, Eq)

newtype ArgumentDef =
  ArgumentDef String
  deriving (Show, Eq)

data Lambda =
  Lambda String [ArgumentDef] [Expr] [Lambda] Flow
  deriving (Show, Eq)

newtype Expr
  = ExpDecl Decl
  -- | ExpCall Call
  deriving (Show, Eq)

-- | Each binding carries the Haskell type of its result (e.g. @"Int32"@) so the
-- printer can annotate it and pin the width. 'DeclConvOp' is the exception: the
-- conversion already names its target type, so it needs no separate annotation.
data Decl
  = DeclBinOp String String BinOp
  | DeclCall String String Call
  | DeclIcmp String String Icmp
  | DeclSelect String String Select
  | DeclConvOp String ConvOp
  -- | An LLVM @freeze@: a typed alias binding @name = value@ (freeze is the
  -- identity in the pure subset). The 'String' is the result type annotation.
  | DeclFreeze String String Value
  deriving (Show, Eq)

-- | A width conversion: the LLVM op name (@trunc@\/@zext@\/@sext@), the source
-- and target bit-widths, and the value being converted.
data ConvOp
  = ConvOp String Int Int Value
  deriving (Show, Eq)

data Select
  = Select Value Value Value
  deriving (Show, Eq)

data Icmp
  = Icmp String Value Value
  deriving (Show, Eq)

data Flow
  = FlowCall Call
  | FlowCond IfThenElse
  deriving (Show, Eq)

data BinOp
  = BinOp String Value Value
  deriving (Show, Eq)

data Call
  = Call Value [Value]
  deriving (Show, Eq)

data IfThenElse
  = IfThenElse Value Call Call
  deriving (Show, Eq)

data Value
  = Const Integer
  | Name String
  | Unit
  deriving (Show, Eq)
