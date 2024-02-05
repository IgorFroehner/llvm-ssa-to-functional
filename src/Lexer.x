{
module Lexer
  ( -- * Invoking Alex
    Alex
  , AlexPosn (..)
  , alexGetInput
  , alexError
  , runAlex
  , alexMonadScan

  , Range (..)
  , RangedToken (..)
  , Token (..)
  , scanMany
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
}

%wrapper "monadUserState-bytestring"

$digit = [0-9]
$alpha = [a-zA-Z]

@id = [\%\@][a-zA-Z0-9\_\$][a-zA-Z0-9\_\$\.]* -- This covers named and unnamed, global or local identifiers

tokens :-

<0> $white+ ;

-- Comment
<0> ";" .*  ;

-- Keywords
<0> define        { tok Define }
<0> declare       { tok Declare }
<0> ret           { tok Return }
<0> type          { tok Typedef }
<0> phi           { tok Phi }
<0> call          { tok Call }
<0> br            { tok Br }
<0> add           { tok Add }
<0> icmp          { tok Icmp }
<0> store         { tok Store }
<0> load          { tok Load }
<0> getelementptr { tok GetElementPtr }

-- Markers / Operators
<0> "="     { tok Assign }
<0> "{"     { tok LCurlyBracket }
<0> "}"     { tok RCurlyBracket }
<0> "("     { tok LPar }
<0> ")"     { tok RPar }
<0> "["     { tok LBrack }
<0> "]"     { tok RBrack }
<0> ","     { tok Comma }

-- Beginning of a block
<0> ($alpha | $digit )+ ":" { tokBasicBlock }

-- Identifiers
<0> @id     { tokId }

-- Types, handling this way for now because I don't know how we're gonna use this
<0> (void | label | i$digit+ | half | float | double | fp128 | ptr) { tokType }

-- Constants
<0> \-?$digit+ { tokInteger }
<0> \"[^\"]*\" { tokString }

-- Comparison kinds

<0> (eq | ne | ugt | uge | ult | ule | sgt | sge | slt | sle) { tokCmp }

-- Ignore for now
<0> "#"$digit+         ;
<0> attributes .*      ;
<0> private            ;
<0> align              ;
<0> inbounds           ;
<0> nsw                ;
<0> nuw                ;
<0> tail               ;
<0> dso_local          ;
<0> noundef            ;
<0> local_unnamed_addr ;
<0> int                ;

{
data AlexUserState = AlexUserState
  {
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState

alexEOF :: Alex RangedToken
alexEOF = do
  (pos, _, _, _) <- alexGetInput
  pure $ RangedToken EOF (Range pos pos)

data Range = Range
  { start :: AlexPosn
  , stop :: AlexPosn
  } deriving (Eq, Show)

data RangedToken = RangedToken
  { rtToken :: Token
  , rtRange :: Range
  } deriving (Eq, Show)

data Token
  -- Identifiers
  = Identifier ByteString
  -- Constants
  | String ByteString
  | Integer Integer
  -- Type
  | Type ByteString
  -- Keywords
  | Define
  | Declare
  | Return
  | Typedef
  | Phi
  | Call
  | Br
  | Add
  | Icmp
  | Store
  | Load
  | GetElementPtr
  -- Basic block
  | BasicBlock ByteString
  -- Markers
  | Assign
  | LCurlyBracket
  | RCurlyBracket
  | LPar
  | RPar
  | LBrack
  | RBrack
  | Comma
  -- Comparison kinds
  | Cmp ByteString
  -- EOF
  | EOF
  deriving (Eq, Show)

mkRange :: AlexInput -> Int64 -> Range
mkRange (start, _, str, _) len = Range{start = start, stop = stop}
  where
    stop = BS.foldl' alexMove start $ BS.take len str

tok :: Token -> AlexAction RangedToken
tok ctor inp len =
  pure RangedToken
    { rtToken = ctor
    , rtRange = mkRange inp len
    }

tokId :: AlexAction RangedToken
tokId inp@(_, _, str, _) len =
  pure RangedToken
    { rtToken = Identifier $ BS.take len str
    , rtRange = mkRange inp len
    }

tokInteger :: AlexAction RangedToken
tokInteger inp@(_, _, str, _) len =
  pure RangedToken
    { rtToken = Integer $ read $ BS.unpack $ BS.take len str
    , rtRange = mkRange inp len
    }

tokString :: AlexAction RangedToken
tokString inp@(_, _, str, _) len =
  pure RangedToken
    { rtToken = String $ BS.take len str
    , rtRange = mkRange inp len
    }

tokType :: AlexAction RangedToken
tokType inp@(_, _, str, _) len =
  pure RangedToken
    { rtToken = Type $ BS.take len str
    , rtRange = mkRange inp len
    }

tokBasicBlock :: AlexAction RangedToken
tokBasicBlock inp@(_, _, str, _) len =
  pure RangedToken
    { rtToken = BasicBlock $ BS.take len str
    , rtRange = mkRange inp len
    }

tokCmp :: AlexAction RangedToken
tokCmp inp@(_, _, str, _) len =
  pure RangedToken
    { rtToken = Cmp $ BS.take len str
    , rtRange = mkRange inp len
    }

scanMany :: ByteString -> Either String [RangedToken]
scanMany input = runAlex input go
  where
    go = do
      output <- alexMonadScan
      if rtToken output == EOF
        then pure [output]
        else (output :) <$> go
}