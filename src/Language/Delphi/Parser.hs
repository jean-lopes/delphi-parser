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

lexeme :: String -> Parser a -> Parser a
lexeme "" p = L.lexeme spaceConsumer p
lexeme xs p = L.lexeme spaceConsumer $ label xs p

signFunction :: Num a => a -> Char -> a
signFunction n '-' = n * (-1)
signFunction n _ = id n

signed :: Num a => Parser a -> Parser a
signed p = lexeme "sign" $ do
    ss <- many (char '+' <|> char '-')
    n <- p
    return $ foldl signFunction n ss

integer :: Parser Int
integer = lexeme "integer" $ some digitChar >>= return . read

real :: Parser Double
real = lexeme "real" $ do
    a <- integer
    _ <- char '.'
    b <- integer
    return $ fromIntegral a + (fromIntegral b / 10)

hexadecimal :: Parser Int
hexadecimal = lexeme "hexadecimal" $ do    
    _ <- char '$'
    hs <- many hexDigitChar
    return $ fst . head $ readHex hs

scientific :: Parser Double
scientific = lexeme "scientific number" $ do
    let integerAsReal = integer >>= return . fromIntegral
    m <- real <|> integerAsReal
    _ <- char' 'e'
    e <- signed integer
    return $ m * 10 ^ e

number :: Parser Delphi.Number
number = lexeme "number" $ choice
    [ try $ signed hexadecimal >>= return . Delphi.Hexadecimal
    , try $ signed scientific >>= return . Delphi.Scientific
    , try $ signed real >>= return . Delphi.Real
    , signed integer >>= return . Delphi.Integer ]

stringConstant :: Parser Delphi.String
stringConstant = do
    ws <- some (char '#' >> integer >>= return . fromIntegral)
    return $ Delphi.StringConstant ws

stringLiteralChar :: Parser Char
stringLiteralChar = nonApostrophe <|> apostrophe
    where apostrophe = char '\'' >> oneOf ['\'']
          nonApostrophe = noneOf ['\'']

stringLiteral :: Parser Delphi.String
stringLiteral = do
    _ <- char '\''
    xs <- many $ try stringLiteralChar
    _ <- char '\''
    return $ Delphi.StringLiteral $ T.pack xs

delphiString :: Parser Delphi.String
delphiString = lexeme "string" $ p >>= return . Delphi.Strings
    where p = some (stringConstant <|> stringLiteral)

{-
identifier :: Parser Identifier
identifier = lexeme $ label "Identifier" $ do
    c <- choice [ char '_', letterChar ]
    cs <- many $ choice [ char '_', alphaNumChar ]
    return $ Identifier $ T.pack $ c:cs
-}