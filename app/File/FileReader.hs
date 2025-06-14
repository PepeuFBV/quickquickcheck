module FileReader (fileRead) where

import System.IO (IOMode (ReadMode), hGetLine, hIsEOF, openFile)
import System.IO.Error (catchIOError)

fileRead :: String -> IO (Either String [(String, String)])
fileRead path =
  catchIOError
    (do
      fileLines <- readFileLines path
      return (Right (findAnnotationsAndFunctions fileLines))
    ) -- no errors
    (\e -> return (Left ("Error reading file: " ++ show e))) -- error string

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

-- finds the annotations and functions in the lines TODO: fix here
findAnnotationsAndFunctions :: [String] -> [(String, String)]
findAnnotationsAndFunctions [] = []
findAnnotationsAndFunctions (currentLine : otherLines) =
  case parseAnnotation currentLine of
    Just annotation ->
      case otherLines of
        (functionLine : moreOtherLines) ->
          if isFunction functionLine
            then
              (annotation, functionLine) : findAnnotationsAndFunctions moreOtherLines
            else findAnnotationsAndFunctions otherLines
        [] -> findAnnotationsAndFunctions otherLines
    Nothing -> findAnnotationsAndFunctions otherLines

-- parses the annotation from the line, extracting everything after the "@quickcheck" annotation
parseAnnotation :: String -> Maybe String
parseAnnotation line =
  case dropWhile (/= "@quickcheck") (words line) of
    foundWordAndRest ->
      if length foundWordAndRest > 1
        then Just (unwords (tail foundWordAndRest)) -- if "@quickcheck" is not the last word, return the rest
        else Nothing

-- checks if the line is a function definition
isFunction :: String -> Bool
isFunction line =
  case words line of
    (name : second : rest) -> not (null name) && "->" `elem` (second : rest) -- checks the normal function definition (name args -> returnType)
    _other -> False