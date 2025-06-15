module Main where

import FileReader (fileRead)

main :: IO ()
main = do
  let path = "app/theorems/Test.hs"
  receiveAnnotationsAndFunctions path

receiveAnnotationsAndFunctions :: FilePath -> IO ()
receiveAnnotationsAndFunctions path = do
  result <- fileRead path
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right fileLine -> mapM_ print fileLine