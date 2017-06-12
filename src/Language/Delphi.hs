{-# LANGUAGE OverloadedStrings #-}
module Language.Delphi(
    module Language.Delphi.AST,
    module Language.Delphi.Code,
    module Language.Delphi.Parser,
    testParser
) where
import Language.Delphi.AST
import Language.Delphi.Code
import Language.Delphi.Parser
import Text.Megaparsec
import Text.Megaparsec.Text

testParser :: Code a => a -> Parser a -> Bool
testParser x p = let xs = code x
                 in case parse p "" xs of
                    (Left _) -> False
                    (Right ast) -> code ast == xs
