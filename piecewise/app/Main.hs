module Main where
import Data.Default (def)
import qualified Data.ByteString as B
import Parser (expr)
import Lexer (alexInput)
import Indentation (runWSLexer)

main :: IO ()
main = do input <- B.getContents
          print $ runWSLexer expr def (alexInput input)
