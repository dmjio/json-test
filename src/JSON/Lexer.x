{
{-# LANGUAGE OverloadedStrings  #-}
--------------------------------------------------------------------------------
-- |
-- Module      : JSON.Lexer
-- Description : Lexer / Tokenizer of a JSON document
-- Maintainer  : David Johnson <david@urbint.com>
-- Maturity    : Usable
--
--------------------------------------------------------------------------------
module JSON.Lexer
  ( getTokens
  , Token (..)
  , Error (..)
  ) where
--------------------------------------------------------------------------------
import           Data.Char     (chr)
import           Control.Monad
import           Data.Bool
import           Text.Read
import           Data.Data
import           GHC.Generics
import           Data.Typeable
import           Data.Scientific
import           Data.Text (Text)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
--------------------------------------------------------------------------------
}

%wrapper "basic-bytestring"

@character = . # \" # \\
@string = \" (@character+)? \"

@whitespace = \x20 | \x09 | \x0A | \x0D

$digit = [0-9]
$negativeSign = \-

@int
  = $digit+
  | $negativeSign $digit+

@nullToken = "null"
@boolToken = "true" | "false"

@symbol
  = "[" | "]"
  | "{" | "}"
  | "(" | ")"
  | ":" | ","

@frac = \. $digit+
@e = "e" | "e+" | "e-" | "E" | "E+" | "E-"
@exp = @e $digit+

@number
  = @int
  | @int @frac
  | @int @exp
  | @int @frac @exp

tokens :-
  @whitespace ;
  @symbol { TokenSymbol }
  @string { TokenString . L.tail . L.init }
  @number { \s ->
    let unpacked = L.unpack s
      in maybe (TokenError $ ConversionError "Not a valid Scientific" s) TokenNumber (readMaybe unpacked) }
  @nullToken { \s -> case s of
    "null" -> TokenNull
    otherwise ->
      TokenError (ConversionError "Not a null value" s)
  }
  @boolToken { \s ->
    case s of
      "true" -> TokenBool True
      "false" -> TokenBool False
      otherwise ->
        TokenError (ConversionError "Invalid boolean value" s)
  }
{

-- | Token unit for lexing the GraphQL specification
data Token
  = TokenInt Int
  | TokenNumber Scientific
  | TokenString ByteString
  | TokenSymbol ByteString
  | TokenBool Bool
  | TokenError Error
  | TokenNull
  deriving (Show, Eq)

-- | Conversion error during lexing phase
data Error
  = ConversionError ByteString ByteString
  deriving (Show, Eq)

-- | Retrieve GraphQL tokens
getTokens :: ByteString -> [Token]
getTokens = alexScanTokens

}
