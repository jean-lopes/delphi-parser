{-# LANGUAGE OverloadedStrings #-}
module Main
where
import Test.QuickCheck
import Text.Megaparsec
import qualified Data.Text as T
import Language.Delphi

main :: IO ()
main = quickCheck prop_identifier

genIdentifier :: Gen Identifier
genIdentifier = do
    x <- frequency
        [ (3, return '_') 
        , (5, elements $ concat [['a'..'z'], ['A'..'Z']]) ]
    xs <- listOf $ frequency
        [ (3, return '_') 
        , (5, elements $ concat [['a'..'z'], ['A'..'Z'], ['0'..'9']]) ]
    return $ Identifier $ T.cons x $ T.pack xs

prop_identifier :: Property
prop_identifier = 
    forAll genIdentifier
        (\ident@(Identifier xs) -> 
            case parse identifier "" (code ident) of
                (Left _) -> False 
                (Right (Identifier ys)) -> ys == xs)
