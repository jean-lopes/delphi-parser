{- | sadsa
-}
module Language.Delphi.AST
where
import qualified Data.Text as T


{- |
    Integer and real constants can be represented in decimal notation
as sequences of digits without commas or spaces, and prefixed with
the + or – operator to indicate sign.

    Values default to positive (so that, for example, 67258 is equivalent
to +67258) and must be within the range of the largest predefined real or
integer type.

Numerals with decimal points or exponents denote reals, while other numerals
denote integers. When the character E or e occurs within a real,
it means “times ten to the power of”. For example, 7E–2 means 7  10^–2,
and 12.25e+6 and 12.25e6 both mean 12.25  10^6.

The dollar-sign prefix indicates a hexadecimal numeral—for example, $8F.
The sign of a hexadecimal is determined by the leftmost (most significant) bit
of its binary representation. /Source: Delphi help/

/EBNF:/

@
    Sign = (\\+|-)?

    Integer = [0-9]+

    Hexadecimal = \\$[0-9a-fA-F]+

    Decimal = [0-9]+\\.[0-9]+

    Exponent = [eE](\\+|-)[0-9]+

    Number = Hexadecimal
           | (Sign) ( Integer | Decimal ) Exponent
           | (Sign) Decimal
           | (Sign) Integer
@
-}
data Number
    -- | Hexadecimal number, valid range: @$8000000000000000@ to @$7FFFFFFFFFFFFFFF@
    = Hexadecimal !Integer
    -- | Scientific notation for a real number: valid range: @19E-4932@ to @19E+4932@
    -- Scientific !Rational
    -- | Real number, valid range: @19E-4932@ to @19E+4932@
    -- Real' !Rational
    -- | Integer number, valid range: @-9223372036854775808@ to @9223372036854775807@
    | Integer' !Integer
    deriving Show

{- |
    Identifiers denote constants, variables, fields, types, properties,
procedures, functions, programs, units, libraries, and packages.

    An identifier can be of any length, but only the first 255 characters
are significant.

    An identifier must begin with a letter or an underscore (_) and cannot 
contain spaces; letters, digits, and underscores are allowed after
the first character.

    Reserved words cannot be used as identifiers. /Source: Delphi help/

/EBNF:/

@
    Identifier = [_a-zA-Z][_a-zA-Z0-9]*
@
-}
newtype Identifier = Identifier T.Text
    deriving Show