module Main where

import Input
import Parser
import Generator

main :: IO ()
main = do 
       input <- sayHello
       if input == '1' then generateDots else parsingFile
