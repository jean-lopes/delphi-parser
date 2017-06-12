{-# LANGUAGE OverloadedStrings #-}
module Language.Delphi.Parser
where
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L
import qualified Data.Text as T
import Language.Delphi.AST

spaceConsumer :: Parser ()
spaceConsumer = L.space (void spaceChar) line block
    where line = L.skipLineComment "\\\\"
          curly_block = L.skipBlockComment "{" "}"
          parens_block = L.skipBlockComment "(*" "*)" 
          block = choice [curly_block, parens_block]

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

identifier :: Parser Identifier
identifier = lexeme $ label "Identifier" $ do
    c <- choice [ char '_', letterChar ]
    cs <- many $ choice [ char '_', alphaNumChar ]
    return $ Identifier $ T.pack $ c:cs

parseWithDefault :: a -> Parser a -> Parser a
parseWithDefault d p = lexeme $ do
    m <- optional p
    case m of
        Nothing  -> return d
        (Just x) -> return x

signed :: Num a => Parser a -> Parser a
signed = L.signed spaceConsumer . lexeme

signedInteger :: Parser Integer
signedInteger = signed L.integer

rationalInteger :: Parser Rational
rationalInteger = L.integer >>= return . fromIntegral

fractionalPart :: Parser Rational
fractionalPart = char '.' >> (/10) <$> rationalInteger

rational :: Parser Rational
rational = do
    i <- rationalInteger
    f <- parseWithDefault 0 fractionalPart
    return $ i + f

{-
signedReal :: Parser Rational
signedReal = signed $ do
    n <- rationalInteger
    f <- fractionalPart
    return $ n + f

--scientific :: Parser (Rational, Int)
scientific :: Parser Rational
scientific = lexeme $ do
    mantissa <- signed rational
    _ <- char' 'e'
    e <- signedInteger >>= return . fromIntegral
    return $ mantissa * 10 ^ e
    --return (mantissa, e)
-}

hexadecimal :: Parser Integer
hexadecimal = lexeme $ char '$' >> L.hexadecimal

number :: Parser Number
number = lexeme $ choice
    [ try $ hexadecimal >>= return . Hexadecimal
    --, try $ scientific >>= \(m, e) -> return $ Scientific m e
    --, try $ scientific >>= return . Scientific
    --, try $ signedReal >>= return . Real'
    , signedInteger >>= return . Integer' ]
