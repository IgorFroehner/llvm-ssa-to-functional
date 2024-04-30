{
module Parser
  ( parseLLVMIR
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (fromJust)
import Data.Monoid (First (..))

import qualified Lexer as L
import Ast
import NameNormalizer
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
  i1         { L.RangedToken (L.Type _) _ }
-- Keywords
  define     { L.RangedToken L.Define _ }
  declare    { L.RangedToken L.Declare _ }
  return     { L.RangedToken L.Return _ }
  typedef    { L.RangedToken L.Typedef _ }
  phi        { L.RangedToken L.Phi _ }
  call       { L.RangedToken L.Call _ }
  br         { L.RangedToken L.Br _ }
  icmp       { L.RangedToken L.Icmp _ }
  store      { L.RangedToken L.Store _ }
  load       { L.RangedToken L.Load _ }
  getelementptr { L.RangedToken L.GetElementPtr _ }
  select     { L.RangedToken L.Select _ }
  -- Binary operations
  binOp      { L.RangedToken (L.BinOp _) _ }
  -- Conversion operations
  convOp     { L.RangedToken (L.ConvOp _) _ }
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
  to        { L.RangedToken L.To _ }
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
  : define typeAnotation gname '(' arguments ')' '{' functionStatementBlocks '}' { FunctionDef (L.rtRange $1 <-> L.rtRange $9) $2 $3 $5 $8 }

funcDec :: { Function L.Range }
  : declare typeAnotation gname '(' arguments ')' { FunctionDec (L.rtRange $1 <-> L.rtRange $6) $2 $3 $5 }

arguments :: { [ArgumentDef L.Range] }
  : arguments ',' argument           { $1 ++ [$3] }
  | argument                         { [$1] }
  |                                  { [] }

argument :: { ArgumentDef L.Range }
  : typeAnotation lname              { ArgumentDef (info $1 <-> info $2) $1 (Just $2) }
  | typeAnotation                    { ArgumentDef (info $1) $1 Nothing }

functionStatementBlocks :: { [BasicBlock L.Range] }
  : initialStatementsBlock blocks    { $1 : $2 }
  | blocks                           { $1 }

blocks :: { [BasicBlock L.Range] }
  : block blocks                     { $1 : $2 }
  |                                  { [] }

block :: { BasicBlock L.Range }
  : blockLabel phiDecs stmts branch  { BasicBlock (info $1 <-> info (head $3)) $1 $2 $3 (Just $4) }
  | blockLabel phiDecs stmts         { BasicBlock (info $1 <-> info (head $2)) $1 [] $3 Nothing }
  | blockLabel phiDecs branch        { BasicBlock (info $1 <-> info (head $2)) $1 $2 [] (Just $3) }
  | blockLabel stmts branch          { BasicBlock (info $1 <-> info (head $2)) $1 [] $2 (Just $3) }
  | blockLabel phiDecs               { BasicBlock (info $1 <-> info (head $2)) $1 $2 [] Nothing }
  | blockLabel branch                { BasicBlock (info $1 <-> info $1) $1 [] [] (Just $2) }
  | blockLabel stmts                 { BasicBlock (info $1 <-> info (head $2)) $1 [] $2 Nothing }

blockLabel :: { Name L.Range }
  : basicblock { unTok $1 (\range (L.BasicBlock label) -> LName range (normalizeName label)) }

initialStatementsBlock :: { BasicBlock L.Range }
  : stmts branch                     { BasicBlock (info (head $1) <-> info (last $1)) (LName (info (head $1)) "a1") [] $1 (Just $2) }
  | branch                           { BasicBlock (info $1) (LName (info $1) "a1") [] [] (Just $1) }
  | stmts                            { BasicBlock (info (head $1) <-> info (last $1)) (LName (info (head $1)) "a1") [] $1 Nothing }

branch :: { Flow L.Range }
  : brCall                           { FlowBranch $1 }
  | ret                              { FlowReturn $1 }

-- Statements

stmts :: { [Stmt L.Range] }
  : stmts stmt                       { $1 ++ [$2] }
  | stmt                             { [$1] }

stmt :: { Stmt L.Range }
  : funcCall                         { SCall $1 }
  | dec                              { SDec $1 }

phiDecs :: { [PhiDec L.Range] }
  : phiDecs phiDec                   { $1 ++ [$2] }
  | phiDec                           { [$1] }

-- Variables and Values

-- Local names are prefixed with a '%'.
lname :: { Name L.Range }
  : lidentifier                      { unTok $1 (\range (L.LIdentifier name) -> LName range (normalizeName name)) }

-- Global names are prefixed with a '@'.
gname :: { Name L.Range }
  : gidentifier                      { unTok $1 (\range (L.GIdentifier name) -> GName range (normalizeOp name)) }

integerValue :: { IntegerValue L.Range }
  : integer                          { unTok $1 (\range (L.Integer value) -> IntegerValue range value) }

value :: { Value L.Range }
  : lname        { ValueName $1 }
  | integerValue { ValueInt $1 }

typeAnotation :: { Type L.Range }
  : type                             { unTok $1 (\range (L.Type typeName) -> Type range (normalizeName typeName)) }

-- Operations

dec :: { Dec L.Range }
  : lname '=' funcCall               { DecCall (info $1 <-> info $3) $1 $3 }
  | lname '=' icmpCall               { DecIcmp (info $1 <-> info $3) $1 $3 }
  | lname '=' binOpCall              { DecBinOp (info $1 <-> info $3) $1 $3 }
  | lname '=' convOpCall             { DecConvOp (info $1 <-> info $3) $1 $3 }
  | lname '=' selectCall             { DecSelect (info $1 <-> info $3) $1 $3 }

phiDec :: { PhiDec L.Range }
  : lname '=' phiCall                { PhiDec (info $1 <-> info $3) $1 $3 }

funcCall :: { Call L.Range }
  : call typeAnotation gname '(' funcCallArguments ')' { unTok $1 (\range _ -> Call range $2 $3 $5) }

funcCallArguments :: { [CallArgument L.Range] }
  : funcCallArguments ',' funcCallArgument      { $1 ++ [$3] }
  | funcCallArgument                            { [$1] }
  |                                             { [] }

funcCallArgument :: { CallArgument L.Range }
  : typeAnotation value               { CallArgument (info $1 <-> info $2) $1 $2 }

phiCall :: { Phi L.Range }
  : phi typeAnotation phiArguments { Phi (L.rtRange $1 <-> info $2) $2 $3 }

phiArguments :: { [(Value L.Range, Name L.Range)] }
  : phiArguments ',' '[' value ',' lname ']' { $1 ++ [($4, $6)] }
  | '[' value ',' lname ']'                  { [($2, $4)] }

ret :: { Return L.Range }
  : return typeAnotation value       { Return (L.rtRange $1 <-> info $3) $2 (Just $3) }
  | return typeAnotation             { Return (L.rtRange $1 <-> info $2) $2 Nothing }

icmpCall :: { Icmp L.Range }
  : icmp cmpDef typeAnotation value ',' value { Icmp (L.rtRange $1 <-> info $6) $2 $3 $4 $6 }

cmpDef :: { Cmp L.Range }
  : cmp                              { unTok $1 (\range (L.Cmp cmp) -> Cmp range (normalizeOp cmp)) }

brCall :: { Br L.Range }
  : br brArguments { Br (L.rtRange $1) $2 }

brArguments :: { [Name L.Range] }
  : typeAnotation lname ',' brArguments                                 { [$2] ++ $4 }
  | typeAnotation lname                                                 { [$2] }

binOpCall :: { BinOpCall L.Range }
  : binOperation typeAnotation value ',' value { BinOpCall (info $1 <-> info $5) $1 $2 $3 $5 }

binOperation :: { BinOp L.Range }
  : binOp { unTok $1 (\range (L.BinOp op) -> BinOp range (normalizeOp op)) }

convOpCall :: { ConvOpCall L.Range }
  : convOperation typeAnotation value to typeAnotation { ConvOpCall (info $1 <-> info $5) $1 $2 $3 $5 }

convOperation :: { ConvOp L.Range }
  : convOp { unTok $1 (\range (L.ConvOp op) -> ConvOp range (normalizeName op)) }

selectCall :: { Select L.Range }
  : select typeAnotation value ',' typeAnotation value ',' typeAnotation value { Select (L.rtRange $1 <-> info $6) $2 $3 $6 $9 }

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
}
