module Language.Delphi.Code
where
import Numeric 
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Language.Delphi.AST

class Code a where
    printCode :: a -> IO ()
    printCode x = T.putStrLn $ code x
    
    code :: a -> T.Text
    

instance Code Identifier where
    code (Identifier xs) = xs

instance Code Number where
    code (Hexadecimal h) = T.cons '$' $ T.toUpper $ T.pack $ showHex h ""
    --code (Scientific s) = T.pack $ showEFloat Nothing (fromRational s) "" -- T.pack $ show m ++ "E" ++ show e
    --code (Real' r) = T.pack $ showFFloat Nothing (fromRat r) ""
    code (Integer' i) = T.pack $ show i



    
