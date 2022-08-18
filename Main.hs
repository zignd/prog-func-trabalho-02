-- Nome: Igor Hipólito Vieira

{-
1. Escreva uma função chamada soma1 que recebe um inteiro como argumento e retorna um
inteiro uma unidade maior que a entrada.
-}
soma1 :: Int -> Int
soma1 x = x + 1

{-
2. Escreva uma função chamada sempre que, não importando o valor de entrada, devolva
sempre zero. Observe que neste caso a entrada pode ser de qualquer tipo.
-}
sempre :: Num x => x -> Int
sempre x = 0

{-
3. Escreva uma função chamada treco que receba três valores em ponto flutuantes com
precisão dupla e retorne o resultado da soma dos dois primeiros multiplicado pelo terceiro.
-}
treco :: Double -> Double -> Double -> Double
treco a b c = (a + b) * c

{-
4. Escreva uma função chamada resto que devolva o resto de uma divisão entre dois números inteiros.
-}
resto :: Int -> Int -> Int
resto x y = mod x y

{-
5. Escreva uma função chamada precoMaior que devolva o maior valor entre quatro valores
monetários.
-}
precoMaior :: Double -> Double -> Double -> Double -> Double
precoMaior a b c d
  | a > b && a > c && a > d = a
  | b > a && b > c && b > d = b
  | c > a && c > b && c > d = c
  | otherwise = d

{-
6. Escreva uma função chamada impar que devolva True, sempre que o resultado do produto
de dois números inteiros for ímpar.
-}
impar :: Int -> Int -> Bool
impar a b
  | mod prod 2 == 1 = True
  | otherwise = False
  where prod = a * b

{-
X. Em Haskell existe o tipo par cuja assinatura tem a seguinte forma: 𝑝𝑎𝑟 ∷ (𝐼𝑛𝑡, 𝐼𝑛𝑡). Escreva
uma função em Haskell que devolva a soma dos componentes de um par de inteiros.
-}
somaPar :: (Int, Int) -> Int
somaPar (a, b) = a + b

{-
7. Escreva uma função em Haskell que receba números reais (double) e devolva o resultado
da equação x^2 + y/2 + z.
-}
equack :: Double -> Double -> Double -> Double
equack x y z = x^2 + y/2 + z

{-
8. Escreva uma função em Haskell chamada diagnostico que receba o peso do aluno e imprima
um diagnóstico de obesidade, segundo a tabela que pode ser encontrada no link:
Sobrepeso, obesidade e obesidade mórbida: entenda a diferença entre os três termos
(cuidadospelavida.com.br). Observe que este diagnóstico é meramente estatístico e não
tem nenhum valor real, está sendo usado nesta questão apenas para a definição das faixas.
Todo e qualquer diagnóstico deve ser feito por um profissional médico.
-}
diagnostico :: Double -> Double -> String
diagnostico peso altura
  | res < 17 = "Muito abaixo do peso"
  | res >= 17 && res < 18.49 = "Abaixo do peso"
  | res >= 18.49 && res < 24.99  = "Peso normal"
  | res >= 24.99 && res < 29.99 = "Sobrepeso"
  | res >= 29.99 && res < 34.99 = "Obesidade leve"
  | res >= 34.99 && res < 39.99 = "Obesidade severa"
  | otherwise = "Obesidade mórbida"
  where res = peso / altura^2

{-
9. Escreva uma função em Haskell chamada bissexto que receba um ano e devolva True se o
ano for bisexto sabendo que anos bissextos obedecem a seguinte regra:
𝑇𝑜𝑑𝑜𝑠 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠𝑒𝑗𝑎𝑚 𝑑𝑖𝑣𝑖𝑠í𝑣𝑒𝑖𝑠 𝑝𝑜𝑟 4
𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 100
𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 400
1997 não é bissexto, 1900 não é bissexto e 2000 é bissexto.
-}
bissexto :: Int -> Bool
bissexto ano
  | mod ano 4 == 0 && (mod ano 100 /= 0 || mod ano 400 == 0) = True
  | otherwise = False

main = do
  putStrLn $ "Func. 1: entrada 3; resultado: " ++ show (soma1 3)
  putStrLn $ "Func. 2: entrada 5.4; resultado: " ++ show (sempre 5.4)
  putStrLn $ "Func. 3: entrada 1.3 2.4 3.7; resultado: " ++ show (treco 1.3 2.4 3.7)
  putStrLn $ "Func. 5: entrada 3.5 6.7 4.1 3.8; resultado: " ++ show (precoMaior 3.5 6.7 4.1 3.8)
  putStrLn $ "Func. 6: entrada 3 3; resultado: " ++ show (impar 3 3)
  putStrLn $ "Func. X: entrada (1, 2); resultado: " ++ show (somaPar (1, 2))
  putStrLn $ "Func. 7: entrada 3 2 1; resultado: " ++ show (equack 3 2 1)
  putStrLn $ "Func. 8: entrada 85.3 1.83; resultado: " ++ show (diagnostico 85.3 1.83)
  putStrLn $ "Func. 9: entrada 2000; resultado: " ++ show (bissexto 2000)
