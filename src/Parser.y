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
  gidentifier { L.RangedToken (L.GIdentifier _) _ }
  lidentifier { L.RangedToken (L.LIdentifier _) _ }
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
  -- Comparison kinds
  cmp       { L.RangedToken (L.Cmp _) _ }

%%

-- Top Level Productions
program :: { [Function L.Range] }
  : program funcDef { $1 ++ [$2] } 
  | funcDef         { [$1] }
  | program funcDec { $1 ++ [$2] }
  | funcDec         { [$1] }

-- Funciton Definitions
funcDef :: { Function L.Range }
  : define typeAnotation gname '(' arguments ')' '{' stmts '}' { FunctionDef (L.rtRange $1 <-> L.rtRange $9) $2 $3 $5 $8 }
  | define typeAnotation gname '(' arguments ')' '{' '}'       { FunctionDef (L.rtRange $1 <-> L.rtRange $8) $2 $3 $5 [] }

funcDec :: { Function L.Range }
  : declare typeAnotation gname '(' arguments ')' { FunctionDec (L.rtRange $1 <-> L.rtRange $6) $2 $3 $5 }

arguments :: { [Argument L.Range] }
  : arguments ',' argument           { $1 ++ [$3] }
  | argument                         { [$1] }
  |                                  { [] }

argument :: { Argument L.Range }
  : typeAnotation lname              { Argument (info $1 <-> info $2) $1 $2 }

-- Statements
stmts :: { [Stmt L.Range] }
  : stmts stmt                       { $1 ++ [$2] }
  | stmt                             { [$1] }

stmt :: { Stmt L.Range }
  : funcCall                         { SCall $1 }
  | dec                              { SDec $1 }
  | ret                              { SReturn $1 }
  | phiCall                          { SPhi $1 }
  | icmpCall                         { SIcmp $1 }

-- Variables and Values

-- Local names are prefixed with a '%'.
lname :: { Name L.Range }
  : lidentifier                      { unTok $1 (\range (L.LIdentifier name) -> LName range name) }

-- Global names are prefixed with a '@'.
gname :: { Name L.Range }
  : gidentifier                      { unTok $1 (\range (L.GIdentifier name) -> GName range name) }

integerValue :: { IntegerValue L.Range }
  : integer                          { unTok $1 (\range (L.Integer value) -> IntegerValue range value) }

value :: { Value L.Range }
  : lname        { ValueName $1 }
  | integerValue { ValueInt $1 }

typeAnotation :: { Type L.Range }
  : type                             { unTok $1 (\range (L.Type typeName) -> Type range typeName) }

-- Operations

dec :: { Dec L.Range }
  : lname '=' funcCall               { Dec (info $1 <-> info $3) $1 $3 }

funcCall :: { Call L.Range }
  : call typeAnotation gname '(' ')' { unTok $1 (\range _ -> Call range $2 $3 []) }

phiCall :: { Phi L.Range }
  : lname '=' phi typeAnotation phiArguments { Phi (info $1 <-> info $4) $1 $4 $5 }

phiArguments :: { [(Value L.Range, Name L.Range)] }
  : phiArguments ',' '[' value ',' lname ']' { $1 ++ [($4, $6)] }
  | '[' value ',' lname ']'                  { [($2, $4)] }

ret :: { Return L.Range }
  : return typeAnotation value       { Return (L.rtRange $1 <-> info $3) $2 (Just $3) }
  | return 'void'                    { Return (L.rtRange $1 <-> L.rtRange $2) (Type (L.rtRange $2) "void") Nothing }

icmpCall :: { Icmp L.Range }
  : icmp cmpDef typeAnotation value ',' value { Icmp (L.rtRange $1 <-> info $6) $2 $3 $4 $6 }

cmpDef :: { Cmp L.Range }
  : cmp                              { unTok $1 (\range (L.Cmp cmp) -> Cmp range cmp) }

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
  = Call a (Type a) (Name a) [Argument a]
  deriving (Foldable, Show)

data Return a
  = Return a (Type a) (Maybe (Value a))
  deriving (Foldable, Show)

data Argument a
  = Argument a (Type a) (Name a)
  deriving (Foldable, Show)

data Dec a
  = Dec a (Name a) (Call a)
  deriving (Foldable, Show)

data Function a
  = FunctionDef a (Type a) (Name a) [(Argument a)] [Stmt a]
  | FunctionDec a (Type a) (Name a) [(Argument a)]
  deriving (Foldable, Show)

data Phi a
  = Phi a (Name a) (Type a) [(Value a, Name a)]
  deriving (Foldable, Show)

data Icmp a
  = Icmp a (Cmp a) (Type a) (Value a) (Value a)
  deriving (Foldable, Show)

data Cmp a
  = Cmp a ByteString
  deriving (Foldable, Show)

data Stmt a
  = SDec (Dec a)
  | SCall (Call a)
  | SReturn (Return a)
  | SPhi (Phi a)  
  | SIcmp (Icmp a)
  deriving (Foldable, Show)
}
