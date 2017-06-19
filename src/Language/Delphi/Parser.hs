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
import Data.List.NonEmpty (NonEmpty(..), fromList)
import Data.List (sort)
import Data.Char (toLower)
import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L
import qualified Data.Text as T
import qualified Language.Delphi.AST as Delphi

spaceConsumer :: Parser ()
spaceConsumer = L.space (void spaceChar) line block
    where 
        line = L.skipLineComment "\\\\"
        curlyBlock = L.skipBlockComment "{" "}"
        parensBlock = L.skipBlockComment "(*" "*)" 
        block = choice [curlyBlock, parensBlock]

lexeme :: Parser a -> Parser a
lexeme p = L.lexeme spaceConsumer p

-- | Apply a parser between parentheses
parens :: Parser a -> Parser a
parens p = between (openClose '(') (openClose ')') p
    where
        openClose = lexeme . char

-- | Apply a parser between brackets
brackets :: Parser a -> Parser a
brackets p = between (openClose '[') (openClose ']') p
    where
        openClose = lexeme . char

stringToData :: [(String, a)] -> Parser a
stringToData xs = choice $ map (\(t, x) -> string' t >> return x) xs
    
-- failOnReservedWord :: Parser a -> Parser a
-- failOnReservedWord p = lexeme $ cs <|> p
--     where 
--         er = unexpected . Label . fromList
--         cs = choice $ map (\s -> string' s >>= er) xs

reserved :: [String]
reserved = sort $ map (map toLower)
    [ "and" , "array", "as", "asm", "begin", "case", "class", "const"
    , "constructor", "destructor", "dispinterface", "div", "do"
    , "except", "exports", "file", "finalization", "finally", "for"
    , "function", "goto", "if", "implementation", "in"  , "inherited"
    , "initialization", "inline", "interface", "is", "label"
    , "library", "mod", "nil", "not", "object", "of", "or", "out"
    , "packed", "procedure", "program", "property", "resourcestring"
    , "set", "shl", "shr", "string", "then", "threadvar", "to", "try"
    , "type", "unit", "until", "uses", "var", "while", "with", "xor"
    , "private", "protected", "public", "published", "automated", "at", "on"
    ]

isReserved :: String -> Bool
isReserved ident = scan reserved
    where
        scan [] = False
        scan (r:rs) = case (compare r ident) of
            LT -> scan rs
            EQ -> True
            GT -> False

identifier :: Parser Delphi.Identifier
identifier = lexeme $ label "identifier" $ do
    c <- choice [ char '_', letterChar ]
    cs <- many $ choice [ char '_', alphaNumChar ]
    let name = c:cs
    if isReserved name
        then unexpected . Label $ fromList $ "reserved word: " ++ name
        else return . Delphi.Identifier $ T.pack name

sign :: Parser Delphi.Sign
sign = lexeme $ stringToData
    [ ("+", Delphi.Plus) 
    , ("-", Delphi.Minus) ]

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
addOp = lexeme $ (sign >>= return . Delphi.SignOp) <|> stringToData
    [ ("or" , Delphi.Or)
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
signed p = lexeme $ label "sign" $ do
    ss <- many (char '+' <|> char '-')
    n <- p
    return $ foldl signFunction n ss

integer :: Parser Int
integer = lexeme $ label "integer" $ some digitChar >>= return . read

real :: Parser Double
real = lexeme $ label "real" $ do
    a <- integer
    _ <- char '.'
    b <- integer
    return $ fromIntegral a + (fromIntegral b / 10)

hexadecimal :: Parser Int
hexadecimal = lexeme $ label "hexadecimal" $ do    
    _ <- char '$'
    hs <- many hexDigitChar
    return $ fst . head $ readHex hs

scientific :: Parser Double
scientific = lexeme $ label "scientific number" $ do
    let integerAsReal = integer >>= return . fromIntegral
    m <- real <|> integerAsReal
    _ <- char' 'e'
    e <- signed integer
    return $ m * 10 ^ e

number :: Parser Delphi.Number
number = lexeme $ label "number" $ choice
    [ try $ signed hexadecimal >>= return . Delphi.Hexadecimal
    , try $ signed scientific >>= return . Delphi.Scientific
    , try $ signed real >>= return . Delphi.Real
    , signed integer >>= return . Delphi.Integer ]

stringConstant :: Parser Delphi.String
stringConstant = do
    (w:ws) <- some (char '#' >> integer <|> hexadecimal >>= return . fromIntegral)
    return $ Delphi.StringConstant $ w :| ws

stringLiteralChar :: Parser Char
stringLiteralChar = nonApostrophe <|> apostrophe
    where apostrophe = char '\'' >> oneOf ['\'']
          nonApostrophe = noneOf ['\'']

stringLiteral :: Parser Delphi.String
stringLiteral = between (char '\'') (char '\'') $ do
    xs <- many $ try stringLiteralChar
    return $ Delphi.StringLiteral $ T.pack xs

delphiString :: Parser Delphi.String
delphiString = lexeme $ label "string" $ p >>= \(x:xs) -> return $ Delphi.Strings $ x :| xs
    where p = some (stringConstant <|> stringLiteral)

constant :: Parser Delphi.Constant
constant = lexeme $ label "constant" $ choice
    [ number >>= return . Delphi.NumericConstant
    , delphiString >>= return . Delphi.TextConstant
    , boolean >>= return . Delphi.BooleanConstant
    ]

identifierList :: Parser Delphi.IdentifierList
identifierList = lexeme $ do
        first <- identifier
        others <- many $ do
            _ <- lexeme $ char ','
            identifier
        return $ Delphi.IdentifierList $ first :| others

unitIdentifier :: Parser Delphi.UnitIdentifier
unitIdentifier = lexeme $
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
labelSection = lexeme $ 
    string' "label" >> labelIdentifier >>= return . Delphi.LabelSection

singleElement :: Parser Delphi.SetElement
singleElement = lexeme $ expression >>= return . Delphi.SingleElement

rangeElement :: Parser Delphi.SetElement
rangeElement = lexeme $ do
    a <- expression
    _ <- string ".."
    b <- expression
    return $ Delphi.RangeElement a b

setElement :: Parser Delphi.SetElement
setElement = lexeme $ try rangeElement <|> singleElement

setConstructor :: Parser Delphi.SetConstructor
setConstructor = lexeme $ brackets $
    many setElement >>= return . Delphi.SetConstructor

designatorFactor :: Parser Delphi.Factor
designatorFactor = lexeme $ do
    d <- designator
    es <- optional expressionList
    return $ Delphi.DesignatorFactor d es

factor :: Parser Delphi.Factor
factor = lexeme $ choice
    [ string' "nil" >> return Delphi.Nil
    , try $ string' "not" >> factor >>= return . Delphi.NotFactor
    , try $ number >>= return . Delphi.NumericFactor
    , try $ delphiString >>= return . Delphi.TextFactor
    , try $ parens expression >>= return . Delphi.ExpressionFactor    
    , try $ setConstructor >>= return . Delphi.SetFactor
    , try designatorFactor
    , typeIdentifier >>= \t -> expression >>= return . Delphi.TypeFactor t
    ]

term :: Parser Delphi.Term
term = lexeme $ do
    f <- factor
    xs <- many $ do
        a <- mulOp
        b <- factor
        return (a, b)
    return $ Delphi.Term f xs

simpleExpression :: Parser Delphi.SimpleExpression
simpleExpression = lexeme $ do
    s <- optional sign
    t <- term
    xs <- many $ do
        a <- addOp
        b <- term
        return (a, b)
    return $ Delphi.SimpleExpression s t xs

expression :: Parser Delphi.Expression
expression = lexeme $ do
    s <- simpleExpression
    xs <- many $ do
        a <- relOp
        b <- simpleExpression
        return (a, b)
    return $ Delphi.Expression s xs

expressionList :: Parser Delphi.ExpressionList
expressionList = lexeme $ do
    (e:es) <- some $ do
        _ <- optional $ char ','
        x <- expression
        return x
    return $ Delphi.ExpressionList $ e :| es

designatorItem :: Parser Delphi.DesignatorItem
designatorItem = lexeme $ choice
    [ try $ identifier >>= return . Delphi.IdentifierDesignator 
    , try $ brackets expressionList >>= return . Delphi.ExpressionListDesignator
    , char '^' >> return Delphi.PointerDesignator
    ]

designator :: Parser Delphi.Designator
designator = lexeme $ do
    q <- qualifiedIdentifier
    ds <- many designatorItem
    return $ Delphi.Designator q ds

constantExpression :: Parser Delphi.ConstantExpression
constantExpression = lexeme $ expression >>= return . Delphi.ConstantExpression