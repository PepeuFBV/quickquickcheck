module FileReader (fileRead) where

import System.IO (IOMode (ReadMode), hGetLine, hIsEOF, openFile)
import System.IO.Error (catchIOError)

-- reads the file at the given path and returns a list of pairs, where each pair contains a function name and all its corresponding annotations
fileRead :: String -> IO (Either String [(String, [String])])
fileRead path =
  catchIOError
    (do
      fileLines <- readFileLines path
      let pairs = findAnnotationsAndFunctions fileLines
          grouped = [(func, anns) | (anns, func) <- pairs] -- groups the annotations with their corresponding functions
      return (Right grouped)
    )
    (\e -> return (Left ("Error reading file: " ++ show e)))

-- reads the file line by line and returns a list of lines
readFileLines :: String -> IO [String]
readFileLines path = do
  handle <- openFile path ReadMode
  let loop list = do
        -- saves the list in the reverse order for O(1) performance
        eof <- hIsEOF handle
        if eof
          then return (reverse list)
          else do
            line <- hGetLine handle
            loop (line : list)
  loop []

-- finds the annotations and functions in the lines
findAnnotationsAndFunctions :: [String] -> [([String], String)]
findAnnotationsAndFunctions = go []
  where
    go _ [] = []
    go anns (line:rest) =
      case parseAnnotation line of
        Just ann -> go (anns ++ [ann]) rest
        Nothing ->
          if isFunction line && not (null anns) -- if the line is a function definition and there are annotations collected
            then (anns, line) : go [] rest -- save the current annotations and reset the list
            else go [] rest -- continue with the next line (next line to be a function or annotation)

-- parses the annotation from the line, extracting everything after the "@quickcheck" annotation
parseAnnotation :: String -> Maybe String
parseAnnotation line =
  case dropWhile (/= "@quickcheck") (words line) of
    (_:rest) | not (null rest) -> Just (unwords rest) -- returns the annotation without the "@quickcheck" part
    _ -> Nothing

-- checks if the line is a function definition
isFunction :: String -> Bool
isFunction line =
  case words line of
    (name : second : rest) -> not (null name) && "->" `elem` (second : rest) -- checks the normal function definition (name args -> returnType)
    _other -> False
    