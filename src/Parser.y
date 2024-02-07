{
{-# LANGUAGE DeriveFoldable, NoStrictData #-}
module Parser
  ( parseLLVMIR
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (fromJust)
import Data.Monoid (First (..))

import qualified Lexer as L
}

%name parseLLVMIR
%tokentype { L.RangedToken }
%error { parseError }
%monad { L.Alex } { >>= } { pure }
%lexer { lexer } { L.RangedToken L.EOF _ }

%token
-- Identifiers
  identifier { L.RangedToken (L.Identifier _) _ }
-- Constants
  string     { L.RangedToken (L.String _) _ }
  integer    { L.RangedToken (L.Integer _) _ }
-- Type
  type       { L.RangedToken (L.Type _) _ }
  'void'     { L.RangedToken (L.Type _) _ }
-- Keywords
  define     { L.RangedToken L.Define _ }
  declare    { L.RangedToken L.Declare _ }
  return     { L.RangedToken L.Return _ }
  typedef    { L.RangedToken L.Typedef _ }
  phi        { L.RangedToken L.Phi _ }
  call       { L.RangedToken L.Call _ }
  br         { L.RangedToken L.Br _ }
  add        { L.RangedToken L.Add _ }
  icmp       { L.RangedToken L.Icmp _ }
  store      { L.RangedToken L.Store _ }
  load       { L.RangedToken L.Load _ }
  getelementptr { L.RangedToken L.GetElementPtr _ }
-- Basic block
  basicblock { L.RangedToken (L.BasicBlock _) _ }
  -- Markers
  '='       { L.RangedToken L.Assign _ }
  '{'       { L.RangedToken L.LCurlyBracket _ }
  '}'       { L.RangedToken L.RCurlyBracket _ }
  '('       { L.RangedToken L.LPar _ }
  ')'       { L.RangedToken L.RPar _ }
  '['       { L.RangedToken L.LBrack _ }
  ']'       { L.RangedToken L.RBrack _ }
  ','       { L.RangedToken L.Comma _ }
  '\n'      { L.RangedToken L.EndOfLine _ }
  -- Comparison kinds
  cmp       { L.RangedToken (L.Cmp _) _ }

%%

program :: { [Function L.Range] }
  : program funcDef { $1 ++ [$2] } 
  | funcDef { [$1] }
  | program funcDec { $1 ++ [$2] }
  | funcDec { [$1] }

funcDef :: { Function L.Range }
  : define name '(' ')' '{' '\n' stmts '}'               { FunctionDef (L.rtRange $1 <-> L.rtRange $8) Nothing $2 $7 }
  | define name '(' ')' '{' '\n' '}'                     { FunctionDef (L.rtRange $1 <-> L.rtRange $6) Nothing $2 [] }
  | define typeAnotation name '(' ')' '{' '\n' stmts '}' { FunctionDef (L.rtRange $1 <-> L.rtRange $9) (Just $2) $3 $8 }
  | define typeAnotation name '(' ')' '{' '\n' '}'       { FunctionDef (L.rtRange $1 <-> L.rtRange $6) (Just $2) $3 [] }

funcDec :: { Function L.Range }
  : declare typeAnotation name '(' ')' { FunctionDec (L.rtRange $1 <-> L.rtRange $5) (Just $2) $3 }
  | declare name '(' ')'               { FunctionDec (L.rtRange $1 <-> L.rtRange $4) Nothing $2 }

stmts :: { [Stmt L.Range] }
  : stmts stmt { $1 ++ [$2] }
  | stmt       { [$1] }

stmt :: { Stmt L.Range }
  : funcCall '\n' { SCall $1 }
  | dec '\n'      { SDec $1 }
  | ret '\n'      { SReturn $1 }

name :: { Name L.Range }
  : identifier { unTok $1 (\range (L.Identifier name) -> Name range name) }

integerValue :: { IntegerValue L.Range }
  : integer { unTok $1 (\range (L.Integer value) -> IntegerValue range value) }

typeAnotation :: { Type L.Range }
  : type { unTok $1 (\range (L.Type typeName) -> Type range typeName) }

dec :: { Dec L.Range }
  : name '=' funcCall { Dec (info $1 <-> info $3) $1 $3 }

funcCall :: { Call L.Range }
  : call name '(' ')'                { unTok $1 (\range _ -> Call range Nothing $2 []) }
  | call typeAnotation name '(' ')'  { unTok $1 (\range _ -> Call range (Just $2) $3 []) }

ret :: { Return L.Range }
  : return value                     { Return (L.rtRange $1 <-> info $2) Nothing (Just $2) }
  | return typeAnotation value       { Return (L.rtRange $1 <-> info $3) (Just $2) (Just $3) }
  | return 'void'                    { Return (L.rtRange $1 <-> L.rtRange $2) (Just (Type (L.rtRange $2) "void")) Nothing }

value :: { Value L.Range }
  : name { ValueName $1 }
  | integerValue { ValueInt $1 }

{
parseError :: L.RangedToken -> L.Alex a
parseError _ = do
  (L.AlexPn _ line column, _, _, _) <- L.alexGetInput
  L.alexError $ "Parse error at line " <> show line <> ", column " <> show column

lexer :: (L.RangedToken -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)

-- | Build a simple node by extracting its token type and range.
unTok :: L.RangedToken -> (L.Range -> L.Token -> a) -> a
unTok (L.RangedToken tok range) ctor = ctor range tok

-- | Unsafely extracts the the metainformation field of a node.
info :: Foldable f => f a -> a
info = fromJust . getFirst . foldMap pure

-- | Performs the union of two ranges by creating a new range starting at the
-- start position of the first range, and stopping at the stop position of the
-- second range.
-- Invariant: The LHS range starts before the RHS range.
(<->) :: L.Range -> L.Range -> L.Range
L.Range a1 _ <-> L.Range _ b2 = L.Range a1 b2

-- * AST

data Name a
  = Name a ByteString
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
  = Call a (Maybe (Type a)) (Name a) [Argument a]
  deriving (Foldable, Show)

data Return a
  = Return a (Maybe (Type a)) (Maybe (Value a))
  deriving (Foldable, Show)

data Argument a
  = Argument a (Name a) (Maybe (Type a))
  deriving (Foldable, Show)

data Dec a
  = Dec a (Name a) (Call a)
  deriving (Foldable, Show)

data Function a
  = FunctionDef a (Maybe (Type a)) (Name a) [Stmt a]
  | FunctionDec a (Maybe (Type a)) (Name a)
  deriving (Foldable, Show)

-- data FunctionDef a
--   = FunctionDef a (Maybe (Type a)) (Name a) [Stmt a]
--   deriving (Foldable, Show)

-- data FunctionDec a
--   = FunctionDec a (Maybe (Type a)) (Name a)
--   deriving (Foldable, Show)

data Stmt a
  = SDec (Dec a)
  | SCall (Call a)
  | SReturn (Return a)
  deriving (Foldable, Show)
}
