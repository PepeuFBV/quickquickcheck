module Main where

import FileReader (fileRead)
import QuickCheckExecute (anotacaoParaComando, executaQuickCheckComHint)

main :: IO ()
main = do
  let path = "app/theorems/Quickquickcheck.hs"
  receiveAnnotationsAndFunctions path
  transformaEmQuickCheck path

-- Identifica as annotations e funções
receiveAnnotationsAndFunctions :: FilePath -> IO ()
receiveAnnotationsAndFunctions path = do
  result <- fileRead path
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right fileLine -> mapM_ print fileLine

-- Parseia para um comando do quickcheck
transformaEmQuickCheck :: FilePath -> IO ()
transformaEmQuickCheck path = do
  result <- fileRead path
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right functions -> mapM_ processFunction functions
  where
    processFunction (funcSig, annotations) = do
      putStrLn $ "\nFunção: " ++ funcSig
      mapM_ processAnnotation annotations

    processAnnotation ann = do
      let cmd = anotacaoParaComando ann
      putStrLn $ "  Executando: " ++ cmd
      executaQuickCheckComHint cmd

