module Main where

import FileReader (fileRead)
import QuickCheckExecute (anotacaoParaComando, executaQuickCheckComHint)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      receiveAnnotationsAndFunctions path
      transformaEmQuickCheck path
    _ -> putStrLn "Uso: quickquickcheck <arquivo.hs>"

-- Apenas imprime o conteúdo lido (opcional)
receiveAnnotationsAndFunctions :: FilePath -> IO ()
receiveAnnotationsAndFunctions path = do
  result <- fileRead path
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right fileLine -> mapM_ print fileLine

-- Para cada função e suas anotações, gera e executa o quickCheck
transformaEmQuickCheck :: FilePath -> IO ()
transformaEmQuickCheck path = do
  result <- fileRead path
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right funcoesEAnotacoes -> mapM_ processFunction funcoesEAnotacoes
  where
    processFunction :: (String, [String]) -> IO ()
    processFunction (funcSig, annotations) = do
      putStrLn $ "\nFunção: " ++ funcSig
      mapM_ processAnnotation annotations

    processAnnotation :: String -> IO ()
    processAnnotation ann = do
      -- gera o comando QuickCheck usando sua função com a nova lógica
      let cmd = anotacaoParaComando ann
      putStrLn $ "  Executando: " ++ cmd
      executaQuickCheckComHint path cmd
