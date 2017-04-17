module Main where
import Data.Default (def)
import qualified Data.ByteString as B
import Parser (expr)
import Lexer (strToInput)
import Indentation (runWSLexer)

main :: IO ()
main = runWSLexer expr def . strToInput <$> B.getContents >>= print
