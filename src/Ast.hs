{-# LANGUAGE DeriveFoldable #-}

module Ast (
  Program(..),
  Name(..),
  IntegerValue(..),
  Value(..),
  Type(..),
  Call(..),
  CallArgument(..),
  Return(..),
  ArgumentDef(..),
  Dec(..),
  Function(..),
  BasicBlock(..),
  Phi(..),
  Icmp(..),
  Cmp(..),
  Br(..),
  BinOp(..),
  BinOpCall(..),
  ConvOp(..),
  ConvOpCall(..),
  Select(..),
  Stmt(..),
  PhiDec(..),
  Flow(..)
) where

newtype Program a
  = Program [Function a]
  deriving (Foldable, Show, Eq)

data Function a
  = FunctionDef a (Type a) (Name a) [ArgumentDef a] [BasicBlock a]
  -- | FunctionDec a (Type a) (Name a) [ArgumentDef a]
  deriving (Foldable, Show, Eq)

data BasicBlock a
  = BasicBlock a (Name a) [PhiDec a] [Stmt a] (Flow a)
  deriving (Foldable, Show, Eq)

newtype Stmt a
  = SDec (Dec a)
  -- | SCall (Call a)
  deriving (Foldable, Show, Eq)

data Flow a
  = FlowBranch (Br a)
  | FlowReturn (Return a)
  deriving (Foldable, Show, Eq)

data Dec a
  = DecCall a (Name a) (Call a)
  | DecIcmp a (Name a) (Icmp a)
  | DecBinOp a (Name a) (BinOpCall a)
  | DecConvOp a (Name a) (ConvOpCall a)
  | DecSelect a (Name a) (Select a)
  deriving (Foldable, Show, Eq)

data PhiDec a
  = PhiDec a (Name a) (Phi a)
  deriving (Foldable, Show, Eq)

data Phi a
  = Phi a (Type a) [(Value a, Name a)]
  deriving (Foldable, Show, Eq)

data Call a
  = Call a (Type a) (Name a) [CallArgument a]
  deriving (Foldable, Show, Eq)

data Icmp a
  = Icmp a (Cmp a) (Type a) (Value a) (Value a)
  deriving (Foldable, Show, Eq)

data BinOpCall a
  = BinOpCall a (BinOp a) (Type a) (Value a) (Value a)
  deriving (Foldable, Show, Eq)

data ConvOpCall a
  = ConvOpCall a (ConvOp a) (Type a) (Value a) (Type a)
  deriving (Foldable, Show, Eq)

data Select a
  = Select a (Type a) (Value a) (Value a) (Value a)
  deriving (Foldable, Show, Eq)

data Return a
  = Return a (Type a) (Maybe (Value a))
  deriving (Foldable, Show, Eq)

newtype Branch a
  = Branch (Br a)
  deriving (Foldable, Show, Eq)

data Name a
  = LName a String
  | GName a String
  deriving (Foldable, Show, Eq)

data IntegerValue a
  = IntegerValue a Integer
  deriving (Foldable, Show, Eq)

data Value a
  = ValueInt (IntegerValue a)
  | ValueName (Name a)
  deriving (Foldable, Show, Eq)

data Type a
  = Type a String
  deriving (Foldable, Show, Eq)

data CallArgument a
  = CallArgument a (Type a) (Value a)
  deriving (Foldable, Show, Eq)

data ArgumentDef a
  = ArgumentDef a (Type a) (Maybe (Name a))
  deriving (Foldable, Show, Eq)

data Cmp a
  = Cmp a String
  deriving (Foldable, Show, Eq)

data Br a
  = Br a [Name a]
  deriving (Foldable, Show, Eq)

data BinOp a
  = BinOp a String
  deriving (Foldable, Show, Eq)

data ConvOp a
  = ConvOp a String
  deriving (Foldable, Show, Eq)
