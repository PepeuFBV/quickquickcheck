module Main where

import Quickquickcheck -- ou o nome do seu módulo

main :: IO ()
main = do
  putStrLn "Testando funções da biblioteca:"
  print (addCommutativa 3 4)
  print (plusOne 7)
  print (dobro 7)