module QuickCheckExecute (anotacaoParaComando, executaQuickCheckComHint, contaArgumentos) where

import Language.Haskell.Interpreter
import Data.Char (isAlpha, isSpace)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, intercalate)

-- FLUXO DO MÓDULO EXECUTE

-- ==================================================================================
-- =  FUNÇÕES PRINCIPAIS                                                            =
-- ==================================================================================

executaQuickCheckComHint :: String -> IO ()
executaQuickCheckComHint comando = do                                                       -- "comando" é o comando no formato do quickcheck que ele executará. Esse comando virá da função anotacaoParaComando
  resultado <- runInterpreter $ do                                                             --
-- *ESSAS 4 LINHAS ABAIXO PROVAVELMENTE SERÃO ALTERADAS AO TRANSFORMAR EM LIB*
    loadModules ["app/theorems/Test.hs"]                                                    -- Carrega o módulo de testes como script
    setTopLevelModules ["Test"]                                                             -- Define o módulo de testes como "usável" (vide documentação hint)
    setImports ["Prelude", "Test.QuickCheck", "Test"]                                       -- Define os imports: Prelude (basicamente tudo do haskell), Quickcheck e onde estão as funções para teste
    interpret comando (as :: IO ())
  case resultado of
    Left err     -> putStrLn $ "Erro ao interpretar: " ++ show err
    Right action -> action


anotacaoParaComando :: String -> String
anotacaoParaComando entrada =
  case detectaOperador entrada of
    Just operador ->
      let (ladoEsquerdo, ladoDireito) = dividirPorOperador entrada operador
          (nomeFuncao, argsComParenteses) = span (/= '(') ladoEsquerdo
          argumentosTexto = takeWhile (/= ')') (drop 1 argsComParenteses)
          argumentos = parseArgs argumentosTexto
          numArgs = contaArgumentos argumentosTexto
          todosVariaveis = all isVariavel argumentos

          -- Monta a expressão esquerda no estilo curried: "f a b c"
          ladoEsquerdoFormatado = unwords (nomeFuncao : argumentos)

          -- Monta o lado direito formatado, respeitando chamadas aninhadas
          ladoDireitoFormatado = formatarLadoDireito ladoDireito

          expressao = ladoEsquerdoFormatado ++ " " ++ operador ++ " " ++ ladoDireitoFormatado

          quickcheckExp
            | numArgs == 1 && isTupla argumentosTexto = "quickCheck (\\(" ++ argumentosParaTupla argumentosTexto ++ ") -> " ++ nomeFuncao ++ " (" ++ argumentosParaTupla argumentosTexto ++ ") " ++ operador ++ " " ++ ladoDireitoFormatado ++ ")"
            | isTupla argumentosTexto = "quickCheck (\\(" ++ argumentosParaTupla argumentosTexto ++ ") -> " ++ expressao ++ ")"
            | todosVariaveis = "quickCheck (\\ " ++ unwords argumentos ++ " -> " ++ expressao ++ ")"
            | otherwise = "quickCheck (" ++ expressao ++ ")"

      in quickcheckExp

    Nothing -> error $ "Anotação inválida para quickcheck: " ++ entrada



-- Conta quantos argumentos estão no texto, separando por vírgula só no nível zero de parênteses
-- Também trata tuplas como múltiplos argumentos
contaArgumentos :: String -> Int
contaArgumentos s =
  let args = separarArgumentos s 0 "" []
  in sum (map contarNaString args)

-- Verifica se o argumento é uma tupla (ex: "(x, y)")
isTupla :: String -> Bool
isTupla str =
  let s = tirarEspacos str
  in head s == '(' && last s == ')' && ',' `elem` s


-- Função auxiliar que separa os argumentos mesmo com tuplas
separarArgumentos :: String -> Int -> String -> [String] -> [String]
separarArgumentos [] _ acc accs = reverse (reverse acc : accs)
separarArgumentos (c:cs) nivel acc accs
  | c == ',' && nivel == 0 = separarArgumentos cs nivel "" (reverse acc : accs)
  | c == '(' = separarArgumentos cs (nivel + 1) (c : acc) accs
  | c == ')' = separarArgumentos cs (nivel - 1) (c : acc) accs
  | otherwise = separarArgumentos cs nivel (c : acc) accs

-- Conta os "elementos" (variáveis) de um argumento
contarNaString :: String -> Int
contarNaString str
  | "(" `isPrefixOf` tirarEspacos str && ")" `isSuffixOf` tirarEspacos str =
      length $ separarArgumentos (init (tail (tirarEspacos str))) 0 "" []
  | otherwise = 1

-- Detecta uma chamada do tipo nomeFuncao(args)
-- Retorna Just (nomeFuncao, argumentosComoString) ou Nothing
detectarChamadaFuncao :: String -> Maybe (String, String)
detectarChamadaFuncao s =
  let s' = tirarEspacos s
      (nome, resto) = span (`notElem` " ()") s'
  in case resto of
    ('(':restoArgs) ->
      let argsTxt = takeWhile (/= ')') restoArgs
      in Just (nome, argsTxt)
    _ -> Nothing

-- Formata o lado direito da expressão garantindo parênteses para chamadas aninhadas
formatarLadoDireito :: [Char] -> String
formatarLadoDireito ladoDireito =
  let s = tirarEspacos ladoDireito
  in if not (null s) && head s == '(' && last s == ')' -- se já tiver parênteses, retorna como está
     then s
     else case detectarChamadaFuncao ladoDireito of
    Just (fNome, argsTxt) ->
      let args = separarArgumentos argsTxt 0 "" []
          argsFormatados = map formatarArgumento args
          argsStr = unwords argsFormatados
      in fNome ++ " " ++ argsStr
    Nothing -> tirarEspacos ladoDireito

-- Formata cada argumento do lado direito, aplicando recursão se for chamada de função
formatarArgumento :: String -> String
formatarArgumento arg =
  case detectarChamadaFuncao arg of
    Just _ -> "(" ++ formatarLadoDireito arg ++ ")"
    Nothing -> tirarEspacos arg


-- ==================================================================================
-- =  FUNÇÕES SUPORTE                                                               =
-- ==================================================================================

-- Detectar operador na lista
detectaOperador :: String -> Maybe String
detectaOperador str =
  let listaOperadoresPossiveis = ["==", "/=", ">=", "<=", ">", "<"]
  in case filter (`isInfixOf` str) listaOperadoresPossiveis of
    (operador:_) -> Just operador
    []     -> Nothing

-- Divide a string da anotação nos dois lados em torno do operador, removendo o operador do resultado
dividirPorOperador :: String -> String -> (String, String)
dividirPorOperador texto operador = procurar "" texto
  where
    procurar :: String -> String -> (String, String)
    procurar antes [] = (antes, "")  -- chegou ao fim sem encontrar
    procurar antes resto
      | operador `isPrefixOf` resto = (antes, drop (length operador) resto)  -- encontrou
      | otherwise = procurar (antes ++ [head resto]) (tail resto)  -- continua procurando

argumentosParaTupla :: String -> String
argumentosParaTupla s = intercalate ", " (parseArgs s)

-- Usa de tirarEspaços para todo elemento do argumento
parseArgs :: String -> [String]
parseArgs input = map tirarEspacos (separarPorVirgula input)
  where
    separarPorVirgula :: String -> [String]
    separarPorVirgula [] = [""]
    separarPorVirgula (c:cs)
      | c == ','  = "":resto
      | otherwise = (c : head resto) : tail resto
      where
        resto = separarPorVirgula cs


-- Retira os espaços de uma string
tirarEspacos :: String -> String
tirarEspacos s = tiraFim (tiraInicio s)
  where
    tiraInicio :: String -> String
    tiraInicio = dropWhile isSpace

    tiraFim :: String -> String
    tiraFim str = reverse (dropWhile isSpace (reverse str))


-- Detecta variáveis
-- Se é tudo letra (alfabeto), retorna True
isVariavel :: String -> Bool
isVariavel [] = False
isVariavel s  = all isAlpha s
-- Limitações: "0,1,2,3,4,5,6,7,8,9", "_", "-"
