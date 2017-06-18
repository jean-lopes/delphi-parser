{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Language.Delphi.Parser
-- Description : Parsers for ASTs
-- Copyright   : (c) Jean Carlo Giambastiani Lopes, 2017
-- License     : MIT
-- Maintainer  : jean.lopes@hotmail.com.br
-- Stability   : experimental
module Language.Delphi.Parser
where
import Numeric
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L
import qualified Data.Text as T
import qualified Language.Delphi.AST as Delphi

spaceConsumer :: Parser ()
spaceConsumer = L.space (void spaceChar) line block
    where line = L.skipLineComment "\\\\"
          curly_block = L.skipBlockComment "{" "}"
          parens_block = L.skipBlockComment "(*" "*)" 
          block = choice [curly_block, parens_block]

lexeme :: Parser a -> Parser a
lexeme p = L.lexeme spaceConsumer p

-- | Labeled lexeme
lexemeL :: String -> Parser a -> Parser a
lexemeL "" p = lexeme p
lexemeL xs p = lexeme $ label xs p

-- | Apply a parser between parentheses
parens :: Parser a -> Parser a
parens p = between (char '(') (char ')') p

stringToData :: [(String, a)] -> Parser a
stringToData xs = choice $ map (\(t, x) -> string' t >> return x) xs
    
classVisibility :: Parser Delphi.ClassVisibility
classVisibility = lexeme $ stringToData
    [ ("public"   , Delphi.Public)
    , ("published", Delphi.Published)
    , ("protected", Delphi.Protected)
    , ("private"  , Delphi.Private)
    ]

ordinalIdentifier :: Parser Delphi.OrdinalIdentifier
ordinalIdentifier = lexeme $ stringToData
    [ ("shortint", Delphi.OrdIdShortInt)
    , ("smallint", Delphi.OrdIdSmallInt)
    , ("integer" , Delphi.OrdIdInteger)
    , ("byte"    , Delphi.OrdIdByte)
    , ("longint" , Delphi.OrdIdLongInt)
    , ("int64"   , Delphi.OrdIdInt64)
    , ("word"    , Delphi.OrdIdWord)
    , ("boolean" , Delphi.OrdIdBoolean)
    , ("char"    , Delphi.OrdIdChar)
    , ("widechar", Delphi.OrdIdWideChar)
    , ("longword", Delphi.OrdIdLongWord)
    , ("pchar"   , Delphi.OrdIdPChar)
    ]

realType :: Parser Delphi.RealType
realType = lexeme $ stringToData
    [ ("real48"  , Delphi.RealTypeReal48)
    , ("real"    , Delphi.RealTypeReal)
    , ("single"  , Delphi.RealTypeSingle)
    , ("double"  , Delphi.RealTypeDouble)
    , ("extended", Delphi.RealTypeExtended)
    , ("currency", Delphi.RealTypeCurrency)
    , ("comp"    , Delphi.RealTypeComp)   
    ]

variantType :: Parser Delphi.VariantType
variantType = lexeme $ stringToData
    [ ("variant"   , Delphi.Variant)
    , ("olevariant", Delphi.OleVariant)
    ]

directive :: Parser Delphi.Directive
directive = lexeme $ stringToData
    [ ("cddecl"     , Delphi.CdDecl)
    , ("register"   , Delphi.Register)
    , ("dynamic"    , Delphi.Dynamic)
    , ("virtual"    , Delphi.Virtual)
    , ("export"     , Delphi.Export)
    , ("external"   , Delphi.External)
    , ("far"        , Delphi.Far)
    , ("forward"    , Delphi.Forward)
    , ("message"    , Delphi.Message)
    , ("override"   , Delphi.Override)
    , ("overload"   , Delphi.Overload)
    , ("pascal"     , Delphi.Pascal)
    , ("reintroduce", Delphi.Reintroduce)
    , ("safecall"   , Delphi.SafeCall)
    , ("stdcall"    , Delphi.StdCall)
    ]

relOp :: Parser Delphi.RelOp
relOp = lexeme $ stringToData
    [ (">" , Delphi.Greater)
    , ("<" , Delphi.Lower)
    , ("<=", Delphi.LowerOrEqual)
    , (">=", Delphi.GreaterOrEqual)
    , ("<>", Delphi.Different)
    , ("in", Delphi.In)
    , ("is", Delphi.Is)
    , ("as", Delphi.As)
    ]

addOp :: Parser Delphi.AddOp
addOp = lexeme $ stringToData
    [ ("+"  , Delphi.Plus)
    , ("-"  , Delphi.Minus)
    , ("or" , Delphi.Or)
    , ("xor", Delphi.Xor)
    ]

mulOp :: Parser Delphi.MulOp
mulOp = lexeme $ stringToData
    [ ("*"  , Delphi.Multiplication)
    , ("/"  , Delphi.Division)
    , ("div", Delphi.Div)
    , ("mod", Delphi.Mod)
    , ("and", Delphi.And)
    , ("shl", Delphi.Shl)
    , ("shr", Delphi.Shr)
    ]

boolean :: Parser Delphi.Boolean
boolean = lexeme $ stringToData
    [ ("true" , Delphi.Boolean True)
    , ("false", Delphi.Boolean False)
    ]

signFunction :: Num a => a -> Char -> a
signFunction n '-' = n * (-1)
signFunction n _ = id n

signed :: Num a => Parser a -> Parser a
signed p = lexemeL "sign" $ do
    ss <- many (char '+' <|> char '-')
    n <- p
    return $ foldl signFunction n ss

integer :: Parser Int
integer = lexemeL "integer" $ some digitChar >>= return . read

real :: Parser Double
real = lexemeL "real" $ do
    a <- integer
    _ <- char '.'
    b <- integer
    return $ fromIntegral a + (fromIntegral b / 10)

hexadecimal :: Parser Int
hexadecimal = lexemeL "hexadecimal" $ do    
    _ <- char '$'
    hs <- many hexDigitChar
    return $ fst . head $ readHex hs

scientific :: Parser Double
scientific = lexemeL "scientific number" $ do
    let integerAsReal = integer >>= return . fromIntegral
    m <- real <|> integerAsReal
    _ <- char' 'e'
    e <- signed integer
    return $ m * 10 ^ e

number :: Parser Delphi.Number
number = lexemeL "number" $ choice
    [ try $ signed hexadecimal >>= return . Delphi.Hexadecimal
    , try $ signed scientific >>= return . Delphi.Scientific
    , try $ signed real >>= return . Delphi.Real
    , signed integer >>= return . Delphi.Integer ]

stringConstant :: Parser Delphi.String
stringConstant = do
    ws <- some (char '#' >> integer <|> hexadecimal >>= return . fromIntegral)
    return $ Delphi.StringConstant ws

stringLiteralChar :: Parser Char
stringLiteralChar = nonApostrophe <|> apostrophe
    where apostrophe = char '\'' >> oneOf ['\'']
          nonApostrophe = noneOf ['\'']

stringLiteral :: Parser Delphi.String
stringLiteral = between (char '\'') (char '\'') $ do
    xs <- many $ try stringLiteralChar
    return $ Delphi.StringLiteral $ T.pack xs

delphiString :: Parser Delphi.String
delphiString = lexemeL "string" $ p >>= return . Delphi.Strings
    where p = some (stringConstant <|> stringLiteral)

constant :: Parser Delphi.Constant
constant = lexemeL "constant" $ choice
    [ number >>= return . Delphi.NumericConstant
    , delphiString >>= return . Delphi.TextConstant
    , boolean >>= return . Delphi.BooleanConstant
    ]

identifier :: Parser Delphi.Identifier
identifier = lexemeL "identifier" $ do
    c <- choice [ char '_', letterChar ]
    cs <- many $ choice [ char '_', alphaNumChar ]
    return $ Delphi.Identifier $ T.pack $ c:cs

identifierList :: Parser Delphi.IdentifierList
identifierList = lexeme $ do
        first <- identifier
        others <- many $ do
            _ <- lexeme $ char ','
            identifier
        return $ Delphi.IdentifierList $ first:others

unitIdentifier :: Parser Delphi.UnitIdentifier
unitIdentifier = lexemeL "unit identifier" $
    identifier >>= return . Delphi.UnitIdentifier

optionalUnitIdAndIdentifier :: Parser (Maybe Delphi.UnitIdentifier, Delphi.Identifier)
optionalUnitIdAndIdentifier = lexeme $ do
    u <- optional $ try $ do 
        x <- unitIdentifier
        _ <- lexeme $ char '.'
        return x
    i <- identifier
    return (u, i)

qualifiedIdentifier :: Parser Delphi.QualifiedIdentifier
qualifiedIdentifier = lexeme $ do
    (u, i) <- optionalUnitIdAndIdentifier
    return $ Delphi.QualifiedIdentifier u i

typeIdentifier :: Parser Delphi.TypeIdentifier
typeIdentifier = lexeme $ do
    (u, i) <- optionalUnitIdAndIdentifier
    return $ Delphi.TypeIdentifier u i

labelIdentifier :: Parser Delphi.LabelIdentifier
labelIdentifier = lexeme $ choice
    [ identifier >>= return . Delphi.IdentifierLabel 
    , integer >>= return . Delphi.IntegerLabel
    ]

labelSection :: Parser Delphi.LabelSection
labelSection = lexeme $ labelIdentifier >>= return . Delphi.LabelSection

