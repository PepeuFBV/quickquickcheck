module QuickCheckExecute (anotacaoParaComando, executaQuickCheckComHint) where


import Language.Haskell.Interpreter
import Data.Char (isAlpha, isSpace)

-- FLUXO DO MÓDULO EXECUTE

-- ==================================================================================
-- =  FUNÇÕES PRINCIPAIS                                                            =
-- ==================================================================================

executaQuickCheckComHint :: String -> IO ()
executaQuickCheckComHint comando = do                                                       -- "comando" é o comando no formato do quickcheck que ele executará. Esse comando virá da função anotacaoParaComando
  resultado <- runInterpreter $ do                                                             --
-- *ESSAS 4 LINHAS ABAIXO PROVAVELMENTE SERÃO ALTERADAS AO TRANSFORMAR EM LIB*
    loadModules ["app/theorems/Quickquickcheck.hs"]                                                    -- Carrega o módulo de testes como script
    setTopLevelModules ["Test"]                                                             -- Define o módulo de testes como "usável" (vide documentação hint)
    setImports ["Prelude", "Test.QuickCheck", "Test"]                                       -- Define os imports: Prelude (basicamente tudo do haskell), Quickcheck e onde estão as funções para teste
    interpret comando (as :: IO ())                                                             
  case resultado of                                                                            
    Left err     -> putStrLn $ "Erro ao interpretar: " ++ show err
    Right action -> action


-- Transforma as anotações em um comando do quickCheck
anotacaoParaComando :: String -> String
anotacaoParaComando entrada = 
  case break (== '=') entrada of                                                            -- Divide no =
    (ladoEsquerdo, '=':'=':ladoDireito) ->
      let (nomeFuncao, argsComParenteses) = span (/= '(') ladoEsquerdo                      -- Divide a string em: nome + argumento
          argumentosTexto = takeWhile (/= ')') $ drop 1 argsComParenteses                   -- nome: tudo antes do "("" + argumento: tudo até o ")"
          argumentos = parseArgs argumentosTexto                                            -- Usa a parseArgs
          todosVariaveis = all isVariavel argumentos                                        -- Usa isVariavel
          chamadaFormatada = unwords (nomeFuncao : argumentos)                              -- Junta nome + argumentos
          expressao = chamadaFormatada ++ " == " ++ tirarEspacos ladoDireito
      in if todosVariaveis
           then "quickCheck (\\ " ++ unwords argumentos ++ " -> " ++ expressao ++ ")"       -- Monta a chamada do quickcheck com ou sem \
           else "quickCheck (" ++ expressao ++ ")"
    _ -> error $ "Anotação inválida para quickcheck: " ++ entrada


-- ==================================================================================
-- =  FUNÇÕES SUPORTE                                                               =
-- ==================================================================================

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
