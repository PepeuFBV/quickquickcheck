module FileReader (fileRead) where

import System.IO (IOMode (ReadMode), hGetLine, hIsEOF, openFile)
import System.IO.Error (catchIOError)

-- reads the file fully and returns the combination of the annotations and functions (or a String error)
fileRead :: String -> IO (Either String [(String, String)])
fileRead path =
  catchIOError
    (Right . findAnnotationsAndFunctions <$> readFileLines path) -- fmap over the findAnnotationsAndFunctions(lines)
    (\e -> return . Left $ "Error reading file: " ++ show e)

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

-- parses the annotation from the line, extracting everything AFTER "qqc:"
parseAnnotation :: String -> Maybe String
parseAnnotation line =
  let found = "@quickcheck"
      ws = words line -- split the line into words
   in case dropWhile (/= found) ws of
        (_ : rest) | not (null rest) -> Just (unwords rest) -- if the found word is not the last one, return the rest of the line
        _ -> Nothing

-- checks if the line is a function definition
isFunction :: String -> Bool
isFunction line =
  let wordsInLine = words line
   in case wordsInLine of
        (name : _ : _) -> not (null name) && "->" `elem` wordsInLine -- check if the line has a function name and an arrow
        _ -> False -- not a function definition