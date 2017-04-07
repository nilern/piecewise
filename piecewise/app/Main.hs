module Main where

import Lib
import Parser

main :: IO ()
main = getContents >>= print . calc . lexer
