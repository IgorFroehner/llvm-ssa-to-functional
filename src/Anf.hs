
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

data Function =
  Function String [ArgumentDef] Lambda Call
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

data Decl
  = DeclBinOp String BinOp
  | DeclCall String Call
  | DeclIcmp String Icmp
  | DeclSelect String Select
  | DeclConvOp String ConvOp
  deriving (Show, Eq)

newtype ConvOp
  = ConvOp Value
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
