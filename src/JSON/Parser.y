{
{-# LANGUAGE OverloadedStrings  #-}
--------------------------------------------------------------------------------
-- |
-- Module      : JSON.Parser
-- Description : Parser for the JSON specification
-- Maintainer  : David Johnson <code@dmj.io>
-- Maturity    : Usable
--
--------------------------------------------------------------------------------
module JSON.Parser
  ( parse
  ) where
--------------------------------------------------------------------------------
import JSON.Lexer
--------------------------------------------------------------------------------
import Data.Aeson (Value(..))
import qualified Data.Vector as V
import Data.Monoid
import Data.Vector (Vector)
import qualified Data.HashMap.Strict as H
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.Text.Encoding (decodeUtf8)
import Data.Text (Text)
--------------------------------------------------------------------------------
}

%name parseJSON Value

%monad { Either String } { (>>=) } { return }

%tokentype { Token }
%error { parseError }

%token
  number	{ TokenNumber $$ }
  string	{ TokenString $$ }
  bool		{ TokenBool $$ }
  null		{ TokenNull }
  ':'           { TokenSymbol ":" }
  ','           { TokenSymbol "," }
  '('           { TokenSymbol "(" }
  ')'           { TokenSymbol ")" }
  '['           { TokenSymbol "[" }
  ']'           { TokenSymbol "]" }
  '{'           { TokenSymbol "{" }
  '}'           { TokenSymbol "}" }

%%

Object :: { H.HashMap Text Value }
  : '{' '}' { mempty }
  | '{' Members '}' { $2 }

Members :: { H.HashMap Text Value }
  : Pair { uncurry H.singleton $1 }
  | Members ',' Pair { uncurry H.singleton $3 <> $1 }

Pair :: { (Text, Value) }
: string ':' Value { (decodeUtf8 (toStrict $1), $3) }

Array :: { Vector Value }
  : '[' ']' { mempty }
  | '[' Elements ']' { $2 }

Elements :: { Vector Value }
  : Value { V.singleton $1 }
  | Elements ',' Value { V.snoc $1 $3 }

Value :: { Value }
  : string { String $ decodeUtf8 (toStrict $1) }
  | number { Number $1 }
  | Object { Object $1 }
  | Array  { Array $1 }
  | bool   { Bool $1 }
  | null   { Null }

{

-- | Parses a JSON 'Value'
parse :: ByteString -> Either String Value
parse = parseJSON . getTokens

parseError :: [Token] -> Either String a
parseError tks =
  Left $ "Parse error: " ++ show tks

}
