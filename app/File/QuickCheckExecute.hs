module QuickCheckExecute (annotationToCommand, runQuickCheckWithHint, countArguments) where

import Language.Haskell.Interpreter
import Data.Char (isAlpha, isSpace)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, intercalate)

runQuickCheckWithHint :: FilePath -> String -> IO ()
runQuickCheckWithHint path command = do
  result <- runInterpreter $ do
    loadModules [path]
    setTopLevelModules ["Test"] -- TODO: change to the module name as the user provides it (only accepts modules named Test for now)
    setImports ["Prelude", "Test.QuickCheck", "Test"] -- TODO: also fix this ^
    interpret command (as :: IO ())
  case result of
    Left err     -> putStrLn $ "Error interpreting: " ++ show err
    Right action -> action

annotationToCommand :: String -> String
annotationToCommand input =
  case detectOperator input of
    Just operator ->
      let (leftSide, rightSide) = splitByOperator input operator
          (functionName, argsWithParens) = span (/= '(') leftSide
          argumentsText = takeWhile (/= ')') (drop 1 argsWithParens)
          arguments = parseArgs argumentsText
          numArgs = countArguments argumentsText
          allVariables = all isVariable arguments

          -- formats the left side in curried style: "f a b c"
          formattedLeftSide = unwords (functionName : arguments)

          -- formats the right side, respecting nested calls
          formattedRightSide = formatRightSide rightSide

          expression = formattedLeftSide ++ " " ++ operator ++ " " ++ formattedRightSide

          quickcheckExp
            | numArgs == 1 && isTuple argumentsText = "quickCheck (\\(" ++ argumentsToTuple argumentsText ++ ") -> " ++ functionName ++ " (" ++ argumentsToTuple argumentsText ++ ") " ++ operator ++ " " ++ formattedRightSide ++ ")"
            | isTuple argumentsText = "quickCheck (\\(" ++ argumentsToTuple argumentsText ++ ") -> " ++ expression ++ ")"
            | allVariables = "quickCheck (\\ " ++ unwords arguments ++ " -> " ++ expression ++ ")"
            | otherwise = "quickCheck (" ++ expression ++ ")"

      in quickcheckExp

    Nothing -> error $ "Invalid annotation for quickcheck: " ++ input

-- counts how many arguments are in the text, splitting by comma only at zero parenthesis level; also treats tuples as multiple arguments
countArguments :: String -> Int
countArguments s =
  let args = splitArguments s 0 "" []
  in sum (map countInString args)

-- checks if the argument is a tuple (e.g., "(x, y)")
isTuple :: String -> Bool
isTuple str =
  let s = trimSpaces str
  in head s == '(' && last s == ')' && ',' `elem` s

-- helper function that splits arguments, even with tuples
splitArguments :: String -> Int -> String -> [String] -> [String]
splitArguments [] _ acc accs = reverse (reverse acc : accs)
splitArguments (c:cs) level acc accs
  | c == ',' && level == 0 = splitArguments cs level "" (reverse acc : accs)
  | c == '(' = splitArguments cs (level + 1) (c : acc) accs
  | c == ')' = splitArguments cs (level - 1) (c : acc) accs
  | otherwise = splitArguments cs level (c : acc) accs

-- counts the "elements" (variables) of an argument
countInString :: String -> Int
countInString str
  | "(" `isPrefixOf` trimSpaces str && ")" `isSuffixOf` trimSpaces str =
      length $ splitArguments (init (tail (trimSpaces str))) 0 "" []
  | otherwise = 1

-- detects a function call of the form functionName(args)
-- returns Just (functionName, argumentsAsString) or Nothing
detectFunctionCall :: String -> Maybe (String, String)
detectFunctionCall s =
  let s' = trimSpaces s
      (name, rest) = span (`notElem` " ()") s'
  in case rest of
    ('(':restArgs) ->
      let argsTxt = takeWhile (/= ')') restArgs
      in Just (name, argsTxt)
    _ -> Nothing

-- formats the right side of the expression, ensuring parentheses for nested calls
formatRightSide :: [Char] -> String
formatRightSide rightSide =
  let s = trimSpaces rightSide
  in if not (null s) && head s == '(' && last s == ')'
     then s
     else case detectFunctionCall rightSide of
    Just (fName, argsTxt) ->
      let args = splitArguments argsTxt 0 "" []
          formattedArgs = map formatArgument args
          argsStr = unwords formattedArgs
      in fName ++ " " ++ argsStr
    Nothing -> trimSpaces rightSide

-- formats each argument on the right side, applying recursion if it's a function call
formatArgument :: String -> String
formatArgument arg =
  case detectFunctionCall arg of
    Just _ -> "(" ++ formatRightSide arg ++ ")"
    Nothing -> trimSpaces arg

-- detects operator in the list
detectOperator :: String -> Maybe String
detectOperator str =
  let possibleOperators = ["==", "/=", ">=", "<=", ">", "<"]
  in case filter (`isInfixOf` str) possibleOperators of
    (operator:_) -> Just operator
    []     -> Nothing

-- splits the annotation string into two sides around the operator, removing the operator from the result
splitByOperator :: String -> String -> (String, String)
splitByOperator text operator = search "" text
  where
    search :: String -> String -> (String, String)
    search before [] = (before, "")
    search before rest
      | operator `isPrefixOf` rest = (before, drop (length operator) rest)
      | otherwise = search (before ++ [head rest]) (tail rest)

argumentsToTuple :: String -> String
argumentsToTuple s = intercalate ", " (parseArgs s)

-- uses trimSpaces for every argument element
parseArgs :: String -> [String]
parseArgs input = map trimSpaces (splitByComma input)
  where
    splitByComma :: String -> [String]
    splitByComma [] = [""]
    splitByComma (c:cs)
      | c == ','  = "":rest
      | otherwise = (c : head rest) : tail rest
      where
        rest = splitByComma cs

-- removes spaces from a string
trimSpaces :: String -> String
trimSpaces s = trimEnd (trimStart s)
  where
    trimStart :: String -> String
    trimStart = dropWhile isSpace

    trimEnd :: String -> String
    trimEnd str = reverse (dropWhile isSpace (reverse str))

-- detects variables, if all letters (alphabet), returns True
isVariable :: String -> Bool
isVariable [] = False
isVariable s  = all isAlpha s
-- limitations: "0,1,2,3,4,5,6,7,8,9", "_", "-"
