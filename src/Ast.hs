{-# LANGUAGE DeriveFoldable, NoStrictData #-}

module Ast where

import Data.ByteString.Lazy.Char8 (ByteString)

data Name a
  = LName a ByteString
  | GName a ByteString
  deriving (Foldable, Show)

data IntegerValue a
  = IntegerValue a Integer
  deriving (Foldable, Show)

data Value a
  = ValueInt (IntegerValue a)
  | ValueName (Name a)
  deriving (Foldable, Show)

data Type a
  = Type a ByteString
  deriving (Foldable, Show)

data Call a
  = Call a (Type a) (Name a) [CallArgument a]
  deriving (Foldable, Show)

data CallArgument a
  = CallArgument a (Type a) (Value a)
  deriving (Foldable, Show)

data Return a
  = Return a (Type a) (Maybe (Value a))
  deriving (Foldable, Show)

data ArgumentDef a
  = ArgumentDef a (Type a) (Maybe (Name a))
  deriving (Foldable, Show)

data Dec a
  = DecCall a (Name a) (Call a)
  | DecIcmp a (Name a) (Icmp a)
  | DecPhi a (Name a) (Phi a)
  | DecBinOp a (Name a) (BinOpCall a)
  | DecConvOp a (Name a) (ConvOpCall a)
  | DecSelect a (Name a) (Select a)
  deriving (Foldable, Show)

data Function a
  = FunctionDef a (Type a) (Name a) [ArgumentDef a] [BasicBlock a]
  | FunctionDec a (Type a) (Name a) [ArgumentDef a]
  deriving (Foldable, Show)

data BasicBlock a
  = BasicBlock a (Maybe (Name a)) [Stmt a]
  deriving (Foldable, Show)

data Phi a
  = Phi a (Type a) [(Value a, Name a)]
  deriving (Foldable, Show)

data Icmp a
  = Icmp a (Cmp a) (Type a) (Value a) (Value a)
  deriving (Foldable, Show)

data Cmp a
  = Cmp a ByteString
  deriving (Foldable, Show)

data Br a
  = Br a [(Type a, Value a)]
  deriving (Foldable, Show)

data BinOp a
  = BinOp a ByteString
  deriving (Foldable, Show)

data BinOpCall a
  = BinOpCall a (BinOp a) (Type a) (Value a) (Value a)
  deriving (Foldable, Show)

data ConvOp a
  = ConvOp a ByteString
  deriving (Foldable, Show)

data ConvOpCall a
  = ConvOpCall a (ConvOp a) (Type a) (Value a) (Type a)
  deriving (Foldable, Show)

data Select a
  = Select a (Type a) (Value a) (Value a) (Value a)
  deriving (Foldable, Show)

data Stmt a
  = SDec (Dec a)
  | SCall (Call a)
  | SReturn (Return a)
  | SBr (Br a)
  deriving (Foldable, Show)
