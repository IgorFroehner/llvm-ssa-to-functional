
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

import Data.ByteString.Lazy.Char8 (ByteString)

newtype Program = Program [Function] deriving Show

data Function =
  Function ByteString [ArgumentDef] Lambda
  deriving Show

newtype ArgumentDef =
  ArgumentDef ByteString
  deriving Show

data Lambda =
  Lambda ByteString [ArgumentDef] [Expr] [Lambda] Flow
  deriving Show

newtype Expr
  = ExpDecl Decl
  -- | ExpCall Call
  deriving Show

data Decl
  = DeclBinOp ByteString BinOp
  | DeclCall ByteString Call
  | DeclIcmp ByteString Icmp
  | DeclSelect ByteString Select
  | DeclConvOp ByteString ConvOp
  deriving Show

newtype ConvOp
  = ConvOp Value
  deriving Show

data Select
  = Select Value Value Value
  deriving Show

data Icmp
  = Icmp ByteString Value Value
  deriving Show

data Flow
  = FlowCall Call
  | FlowCond IfThenElse
  deriving Show

data BinOp
  = BinOp ByteString Value Value
  deriving Show

data Call
  = Call Value [Value]
  deriving Show

data IfThenElse
  = IfThenElse Value Call Call
  deriving Show

data Value
  = Const Integer
  | Name ByteString
  | Unit
  deriving Show
