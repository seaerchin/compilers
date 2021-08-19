module Regex where

import Control.Applicative
import Data.Char (toUpper)
import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.Char (letter)

-- Describes the set of regex expressions
data Regex = Expr String | Alt Regex Regex | Concat Regex Regex | Closure Regex | Parens Regex | Pow Regex | Not Regex | Choice Regex | Single Regex

lowercase = ['a' .. 'z']

uppercase = map toUpper lowercase

numbers = ['0' .. '9']

expr :: ParsecT [Char] () Identity Regex
expr = Expr <$> Text.Parsec.many alphaNum

parseAlt = undefined
