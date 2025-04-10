module Parser where
import Lang
import Text.ParserCombinators.Parsec

import Control.Applicative ((<|>), many, (<$>), (<*>))
import Data.Char (isAlpha, isDigit, isSpace)

data Token
    = TIdent String
    | TNum Int
    | TLet Bool -- True for letrec, False for let
    | TIn
    | TCase
    | TOf
    | TLam -- backslash
    | TPack
    | TLBrace
    | TRBrace
    | TLParen
    | TRParen
    | TLAngle
    | TRAngle
    | TArrow
    | TAssign
    | TSemicolon
    | TBinOp String
    deriving (Show, Eq)


-- lex
