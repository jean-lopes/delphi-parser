-- |
-- Module      : Language.Delphi.Code
-- Description : Class and instances to generate code from AST
-- Copyright   : (c) Jean Carlo Giambastiani Lopes, 2017
-- License     : MIT
-- Maintainer  : jean.lopes@hotmail.com.br
-- Stability   : experimental
module Language.Delphi.Code
where
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Language.Delphi.AST

-- | Instances of this class will implement code generation. (AST -> Code)
class Code a where
    -- | Prints the code
    printCode :: a -> IO ()
    printCode = T.putStrLn . code
    
    -- | Generates code
    code :: a -> T.Text

-- | Identifier instance
instance Code Identifier where
    code (Identifier xs) = xs

-- | Number instance
--instance Code Number where
    --code (Hexadecimal h) = T.cons '$' $ T.toUpper $ T.pack $ showHex h ""
    --code (Scientific s) = T.pack $ showEFloat Nothing (fromRational s) "" -- T.pack $ show m ++ "E" ++ show e
    --code (Real' r) = T.pack $ showFFloat Nothing (fromRat r) ""
    --code (Integer' i) = T.pack $ show i



    
