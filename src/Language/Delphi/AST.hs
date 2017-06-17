-- |
-- Module      : Language.Delphi.AST
-- Description : Delphi Abstract Syntax Tree representation
-- Copyright   : (c) Jean Carlo Giambastiani Lopes, 2017
-- License     : MIT
-- Maintainer  : jean.lopes@hotmail.com.br
-- Stability   : experimental
--
-- = Abstract Syntax Tree
-- 
-- Everything is __case-insensitive__ in delphi, thus, the EBNF will only show
-- lower-case letters.
-- 
-- this module is supposed to be imported qualified, suggested alias:
-- 
-- > import qualified Language.Delphi.AST as Delphi
-- 
-- /Base EBNF:/
-- 
-- > character = ? any character ?;
-- >            
-- > letter = 'a' | 'b' | 'c' | 'd' | 'e' | 'f'
-- >        | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' 
-- >        | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' 
-- >        | 's' | 't' | 'u' | 'v' | 'w' | 'x' 
-- >        | 'y' | 'z' ;
-- >
-- > digit = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9';
-- >
-- > sign = '+' | '-';
module Language.Delphi.AST
where
import Prelude(Show, Int, Double, Maybe, Bool)
import Data.Word (Word8)
import qualified Data.Text as T

-- | /EBNF:/
--
-- > class-visibility = 'public'
-- >                  | 'published'
-- >                  | 'protected'
-- >                  | 'private'
-- >                  ;
data ClassVisibility 
    = Public
    | Published
    | Protected
    | Private
    deriving Show

-- | /EBNF:/
--
-- > ordinal-identifier = 'shortint' 
-- >                    | 'smallint'
-- >                    | 'integer'
-- >                    | 'byte'
-- >                    | 'longint'
-- >                    | 'int64'
-- >                    | 'word'
-- >                    | 'boolean'
-- >                    | 'char'
-- >                    | 'widechar'
-- >                    | 'longword'
-- >                    | 'pchar'
-- >                    ;
data OrdinalIdentifier
    = OrdIdShortInt
    | OrdIdSmallInt
    | OrdIdInteger
    | OrdIdByte
    | OrdIdLongInt
    | OrdIdInt64
    | OrdIdWord
    | OrdIdBoolean
    | OrdIdChar
    | OrdIdWideChar
    | OrdIdLongWord
    | OrdIdPChar
    deriving Show

-- | /EBNF:/
--
-- > real-type = 'real48'
-- >           | 'real'
-- >           | 'single'
-- >           | 'double'
-- >           | 'extended'
-- >           | 'currency'
-- >           | 'comp'
-- >           ;
data RealType
    = RealTypeReal48
    | RealTypeReal
    | RealTypeSingle
    | RealTypeDouble
    | RealTypeExtended
    | RealTypeCurrency
    | RealTypeComp
    deriving Show

-- | /EBNF:/
--
-- > variant-type = 'variant' | 'olevariant' ;
data VariantType 
    = Variant
    | OleVariant
    deriving Show

-- | /EBNF:/
--
-- > directive = 'cddecl'
-- >           | 'register'
-- >           | 'dynamic'
-- >           | 'virtual'
-- >           | 'export'
-- >           | 'external'
-- >           | 'far'
-- >           | 'forward'
-- >           | 'message'
-- >           | 'override'
-- >           | 'overload'
-- >           | 'pascal'
-- >           | 'reintroduce'
-- >           | 'safecall'
-- >           | 'stdcall'
-- >           ;
data Directive
    = CdDecl
    | Register
    | Dynamic
    | Virtual
    | Export
    | External
    | Far
    | Forward
    | Message
    | Override
    | Overload
    | Pascal
    | Reintroduce
    | SafeCall
    | StdCall
    deriving Show

-- | /EBNF:/
--
-- > rel-op = '>' | '<' | '<=' | '>=' | '<>' | 'IN' | 'IS' | 'AS' ;
data RelOp
    = Greater
    | Lower
    | LowerOrEqual
    | GreaterOrEqual
    | Different
    | In
    | Is
    | As
    deriving Show

-- | /EBNF:/
--
-- > add-op = '+' | '-' | 'or' | 'xor' ;
data AddOp
    = Plus
    | Minus
    | Or
    | Xor
    deriving Show

-- | /EBNF:/
--
-- > mul-op = '*' | '/' | 'div' | 'mod' | 'and' | 'shl' | 'shr' ;
data MulOp
    = Multiplication
    | Division
    | Div
    | Mod
    | And
    | Shl
    | Shr
    deriving Show

-- | /EBNF:/
--
-- > boolean = 'true' | 'false' ;
newtype Boolean = Boolean Bool
    deriving Show

-- | /EBNF:/
--
-- > integer = digit, { digit };
-- >
-- > hexadecimal-digit = digit | 'a' | 'b' | 'c' | 'd' | 'e' | 'f';
-- >
-- > hexadecimal = '$', hexadecimal-digit, { hexadecimal-digit };
-- >
-- > real = integer, '.', integer;
-- >
-- > scientific = real, 'e', [ sign ], integer;
-- >
-- > number = { sign }, ( hexadecimal | scientific | real | integer );
data Number
    -- | Hexadecimal number, examples:
    --
    -- > $1, $FF, -$1
    = Hexadecimal !Int
    -- | Scientific notation for a real number, examples:
    --
    -- > 1E7, 1.2E-4, 1E+1, -1E3
    | Scientific !Double
    -- | Real number, examples:
    --
    -- > 1.0, 1.5, 0.5, -1.5
    | Real !Double
    -- | Integer number, examples:
    -- 
    -- > 1, 2, 3, -1, -15
    | Integer !Int
    deriving Show

-- | /EBNF:/
--
-- > character-constant = '#', digit, { digit };
-- >
-- > string-constant = character-constant, { character-constant };
-- >
-- > string-literal = "'", { (character - "'") | "''" }, "'";
-- > 
-- > any-string = string-constant | string-literal;
-- >
-- > string = any-string, { any-string };
data String 
    -- | A String composed from byte values, examples:
    --
    -- > #65       -> A
    -- > #89       -> Y
    -- > #65#89#89 -> AYY
    = StringConstant [Word8]
    -- | A String literal, is your common quoted string, the only escaped
    --   character is the apostrophe, examples:
    --
    -- > ''     -> (null)
    -- > 'AYY'  -> AYY
    -- > ''''   -> '    
    -- > 'I''m' -> I'm
    | StringLiteral !T.Text
    -- | The string types can be mixed, hence this constructor, examples:
    --
    -- > 'AYYY'#32'LMAO' -> Strings [ StringLiteral ""
    -- >                            , StringConstant [32]
    -- >                            , StringLiteral ""]
    | Strings [String]
    deriving Show

-- | /EBNF:/
--
-- > constant = number | string | boolean ;
data Constant
    = NumericConstant !Number
    | TextConstant !String
    | BooleanConstant !Boolean
    deriving Show

-- | /EBNF:/
--
-- > alphanumeric = digit | letter;
-- >
-- > identifier = ('_' | letter), { '_' | alphanumeric };
newtype Identifier
    = Identifier T.Text
    deriving Show

-- | /EBNF:/
--
-- > identifier-list = identifier, { ',', identifier };
newtype IdentifierList
    = IdentifierList [Identifier]
    deriving Show

-- | /EBNF:/
--
-- > unit-identifier = identifier;
newtype UnitIdentifier
    = UnitIdentifier Identifier
    deriving Show

-- | /EBNF:/
--
-- > qualified-identifier = [ unit-identifier, '.' ], identifier;
data QualifiedIdentifier
    = QualifiedIdentifier (Maybe UnitIdentifier) Identifier
    deriving Show

-- | /EBNF:/
--
-- > type-identifier = [ unit-id, '.' ], identifier;
data TypeIdentifier
    = TypeIdentifier (Maybe UnitIdentifier) Identifier
    deriving Show

-- | /EBNF:/
--
-- > label-id = identifier | integer;
data LabelIdentifier
    = IdentifierLabel Identifier
    | IntegerLabel Int
    deriving Show

-- | /EBNF:/
--
-- > label-section = 'label', label-id;
newtype LabelSection
    = LabelSection LabelIdentifier
    deriving Show
