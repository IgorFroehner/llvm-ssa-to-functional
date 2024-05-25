
module Anf (
  Program(..),
  Function(..),
  ArgumentDef(..),
  Let(..),
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

newtype Program = Program [Function] deriving Show

data Function =
  Function String [ArgumentDef] Let
  deriving Show

newtype ArgumentDef =
  ArgumentDef String
  deriving Show

data Let =
  Let String [ArgumentDef] [Expr] [Let] Flow
  deriving Show

data Expr
  = ExpCall Call
  | ExpDecl Decl
  deriving Show

data Decl
  = DeclBinOp String BinOp
  | DeclCall String Call
  | DeclIcmp String Icmp
  | DeclSelect String Select
  | DeclConvOp String ConvOp
  deriving Show

newtype ConvOp
  = ConvOp Value
  deriving Show

data Select
  = Select Value Value Value
  deriving Show

data Icmp
  = Icmp String Value Value
  deriving Show

data Flow
  = FlowCall Call
  | FlowCond IfThenElse
  deriving Show

data BinOp
  = BinOp String Value Value
  deriving Show

data Call
  = Call Value [Value]
  deriving Show

data IfThenElse
  = IfThenElse Value Call Call
  deriving Show

data Value
  = Const Integer
  | Name String
  | Unit
  deriving Show
