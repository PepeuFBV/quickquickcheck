module Test
  ( plusOne, incremento, dobro, addCommutativa, addAssociativa, multComutativa, identidade, quadradoSemprePositivo, dobroErrado,
  fatorial, estaVazio, stringLength, reverseConcat, ePar, cabecaSingleton, ordenarPreservaTamanho, quicksort
  )
where

-- @quickcheck plusOne(x) == x + 1
-- @quickcheck plusOne(0) == 1
-- @quickcheck plusOne(1) == 2
plusOne :: Integer -> Integer
plusOne x = x + 1
-- A função `plusOne` incrementa um número inteiro em 1.
-- Os testes do QuickCheck verificam se `plusOne x` é sempre igual a `x + 1`, inclusive para casos específicos como 0 e 1.

-- @quickcheck incremento(x) > x
incremento :: Int -> Int
incremento x = x + 1
-- A função `incremento` soma 1 ao número fornecido.
-- O teste garante que o resultado é sempre maior do que o número original.

-- @quickcheck dobro(x) == x + x
dobro :: Int -> Int
dobro x = x * 2
-- A função `dobro` retorna o dobro do número.
-- O teste confirma se `dobro x` é igual a `x + x`.

-- @quickcheck addCommutativa(x, y) == addCommutativa(y, x)
addCommutativa :: Int -> Int -> Int
addCommutativa x y = x + y
-- Soma dois inteiros.
-- O teste verifica a propriedade comutativa da adição: `x + y == y + x`.

-- @quickcheck addAssociativa(x, y, z) == addAssociativa(y, z, x)
addAssociativa :: Int -> Int -> Int -> Int
addAssociativa x y z = x + (y + z)
-- Soma três inteiros com parênteses à direita.
-- O teste confere se a propriedade associativa se mantém

-- @quickcheck multComutativa(x, y) == y * x
multComutativa :: Int -> Int -> Int
multComutativa x y = x * y
-- Multiplica dois inteiros.
-- O teste garante que a multiplicação é comutativa.

-- @quickcheck identidade(x) == x
identidade :: Int -> Int
identidade x = x + 0
-- Retorna o mesmo número que recebe.
-- O teste verifica se somar 0 mantém o valor original (identidade da adição).

-- @quickcheck quadradoSemprePositivo(x) >= 0
quadradoSemprePositivo :: Int -> Int
quadradoSemprePositivo x = x * x
-- Retorna o quadrado de um número.
-- O teste verifica se o resultado é sempre maior ou igual a 0, o que é uma propriedade dos quadrados.

-- @quickcheck dobroErrado(x) == x + x
dobroErrado :: Int -> Int
dobroErrado x = x + x + 1  -- propositalmente errado para testar falha
-- Aqui há um erro proposital: deveria ser `x + x`, mas soma 1 a mais.
-- O teste garante que o QuickCheck é capaz de detectar falhas em propriedades declaradas incorretamente.

-- @quickcheck fatorial(0) == 1
-- @quickcheck fatorial(5) == 120
fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial (n - 1)
-- Calcula o fatorial de um número.
-- Os testes verificam valores fixos, como fatorial de 0 e 5.
-- Obs: valores negativos entram em recursão infinita se não forem filtrados. ODMFNOJEFIWENBALSKD

-- @quickcheck estaVazio([]) == True
estaVazio :: [Int] -> Bool
estaVazio = null
-- Verifica se uma lista está vazia.
-- O teste garante que `estaVazio []` retorna `True`.

-- @quickcheck stringLength(s) == length s
stringLength :: String -> Int
stringLength s = length s
-- Retorna o tamanho de uma string.
-- O teste compara com o `length` padrão, verificando se a função retorna o valor correto.

-- @quickcheck reverseConcat(xs, ys) == reverse ys ++ reverse xs
reverseConcat :: [Int] -> [Int] -> [Int]
reverseConcat xs ys = reverse (xs ++ ys)
-- Inverte a concatenação de duas listas.
-- O teste valida a propriedade `reverse (xs ++ ys) == reverse ys ++ reverse xs`.

-- @quickcheck ePar(x) == (x `mod` 2 == 0)
ePar :: Int -> Bool
ePar x = x `mod` 2 == 0
-- A função `ePar` verifica se um número é par usando a operação de módulo.
-- O teste do QuickCheck confirma que `ePar x` retorna `True` se e somente se o resto da divisão de `x` por 2 for zero.

-- @quickcheck cabecaSingleton(x) == x
cabecaSingleton :: Int -> Int
cabecaSingleton x = head [x]
-- Retorna o primeiro elemento de uma lista contendo apenas `x`.
-- O teste valida que `head [x]` realmente retorna `x`.

-- @quickcheck ordenarPreservaTamanho(xs) == length xs
ordenarPreservaTamanho :: [Int] -> Int
ordenarPreservaTamanho xs = length (quicksort xs)
-- Ordena uma lista usando o algoritmo de quicksort e retorna seu tamanho.
-- O teste garante que ordenar a lista não altera seu comprimento.


{- 
Esta função foi produziaa com o auxílio do ChatGPT, Conforme exigência da disciplina para fins de transparência acadêmica.

Prompt utilizado:
"Gere a função do quicksort auxiliar de 'ordenarPreservaTamanho'"
-}
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x]
                 ++ [x]
                 ++ quicksort [y | y <- xs, y > x]
-- Função auxiliar para "ordenarPreservaTamanho"

{-
  Este bloco de código foi produzido com auxílio do ChatGPT, conforme exigência da disciplina para fins de transparência acadêmica.

  Pompt utilizado:
  "Dê exemplos de funções matemáticas que podem ser testadas com QuickCheck em Haskell. 
  Exemplos que envolvam propriedades matemáticas, manipulação de listas/strings etc. Me dê ideias que sejam variadas e boas para um projeto.".

  A implementação, em sua maioria, do código, ajustes e comentários finais foram realizados manualmente pelo grupo.
-}

