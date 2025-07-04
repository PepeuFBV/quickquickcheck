module Main where

import FileReader (fileRead)
import QuickCheckExecute (annotationToCommand, runQuickCheckWithHint)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      -- printAnnotationsAndFunctions path
      transformToQuickCheck path
    _ -> putStrLn "Usage: quickquickcheck <file.hs>"

-- just prints the read content (optional)
printAnnotationsAndFunctions :: FilePath -> IO ()
printAnnotationsAndFunctions path = do
  result <- fileRead path
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right fileLine -> mapM_ print fileLine

-- for each function and its annotations, generates and runs quickCheck
transformToQuickCheck :: FilePath -> IO ()
transformToQuickCheck path = do
  result <- fileRead path
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right functionsAndAnnotations -> printFunctions functionsAndAnnotations
  where
    printFunctions [] = return ()
    printFunctions [lastFunc] = processFunction lastFunc  -- no extra blank line after the last function
    printFunctions (f:fs) = do
      processFunction f
      putStrLn ""  -- blank line
      printFunctions fs

    processFunction :: (String, [String]) -> IO ()
    processFunction (funcSig, annotations) = do
        putStrLn $ "Function: " ++ funcSig
        mapM_ processAnnotation annotations
        hFlush stdout -- ensures output is printed immediately

    processAnnotation :: String -> IO ()
    processAnnotation ann = do
      -- generates the QuickCheck command using your function with the new logic
      let cmd = annotationToCommand ann
      putStrLn $ "  Running: " ++ cmd
      runQuickCheckWithHint path cmd
      putStrLn ""  -- always prints a blank line after each test
      