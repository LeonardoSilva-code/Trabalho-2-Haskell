-- Leonardo Silva de Abreu

{-
1. Escreva uma função chamada soma1 que recebe um inteiro como argumento e retorna um inteiro uma unidade maior que a entrada.  
-}
soma1 :: Int -> Int
soma1 n = n + 1

{-
2. Escreva  uma  função  chamada  sempre  que,  não  importando  o  valor  de  entrada,  devolva sempre zero. Observe que neste caso a entrada pode ser de qualquer tipo. 
-}
sempre :: n -> Int
sempre n = 0

{-
3. Escreva  uma  função  chamada  treco  que  receba  três  valores  em  ponto  flutuantes  com precisão dupla e retorne o resultado da soma dos dois primeiros multiplicado pelo terceiro. 
-}
treco :: Double -> Double -> Double -> Double
treco x y z = (x+y) * z

{-
4. Escreva uma função chamada resto que devolva o resto de uma divisão entre dois números 
inteiros. 
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
        | d > a && d > b && d > c = d
        | otherwise = a

{-
6. Escreva uma função chamada impar que devolva True, sempre que o resultado do produto de dois números inteiros for ímpar.  
-}
impar :: Int -> Int -> Bool
impar x y 
    | mod (x*y) 2 == 1 = True
    | otherwise = False

{-
Em Haskell existe o tipo par cuja assinatura tem a seguinte forma: 𝑝𝑎𝑟∷(𝐼𝑛𝑡,𝐼𝑛𝑡). Escreva uma função em Haskell que devolva a soma dos componentes de um par de inteiros. 
-}
somaPar :: (Int, Int) -> Int
somaPar (x,y) = x + y

{-
7. Escreva uma função em Haskell que receba números reais (double) e devolva o resultado da equação 𝑥2 +𝑦2 +𝑧.
-}
equacao :: Double -> Double -> Double -> Double
equacao x y z = (x^2) + (y/2) + z

{-
8. Escreva uma função em Haskell chamada diagnostico que receba o peso do aluno e imprima 
um  diagnóstico  de  obesidade,  segundo  a  tabela  que  pode  ser  encontrada  no  link: 
Sobrepeso,  obesidade  e  obesidade  mórbida:  entenda  a  diferença  entre  os  três  termos 
(cuidadospelavida.com.br).  Observe  que  este  diagnóstico  é  meramente  estatístico  e  não 
tem nenhum valor real, está sendo usado nesta questão apenas para a definição das faixas. 
Todo e qualquer diagnóstico deve ser feito por um profissional médico.  
-}
indiceIMC :: Double -> Double -> String
indiceIMC peso altura
              | peso/(altura^2) < 17 = "Abaixo de 17  -  Muito abaixo do peso"
              | peso/(altura^2) < 18.49 = "Entre 17 e 18,49   -  Abaixo do peso"
              | peso/(altura^2) < 24.99 = "Entre 18,5 e 24,99  -    Peso normal"
              | peso/(altura^2) < 29.99 = "Entre 25 e 29,99   -  Sobrepeso"
              | peso/(altura^2) < 34.99 = "Entre 30 e 34,99   -  Obesidade Ieve"
              | peso/(altura^2) < 39.99 = "Entre 35 e 39,99   -  Obesidade severa"
              | otherwise = "Acima de 40 - Obesidade morbida"

{-
9. Escreva uma função em Haskell chamada bissexto que receba um ano e devolva True se o ano for bisexto sabendo que anos bissextos obedecem a seguinte regra:  
𝑇𝑜𝑑𝑜𝑠 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠𝑒𝑗𝑎𝑚 𝑑𝑖𝑣𝑖𝑠í𝑣𝑒𝑖𝑠 𝑝𝑜𝑟 4 
      𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 100 
            𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 400 
1997 não é bissexto, 1900 não é bissexto e 2000 é bissexto. 
-}
bissexto :: Int -> Bool
bissexto ano 
          | mod ano 100 == 0 && mod ano 400 == 0 = True
          | mod ano 100 == 0 = False
          | mod ano 400 == 0 = False
          | mod ano 4 == 0 = True
          | otherwise = False


main = do
putStrLn "TRABALHO 2. INTRODUÇÃO LINGUAGEM HASKELL"

--soma1 teste
let soma1Input = 5
let soma1Resultado = soma1 soma1Input
let soma1TesteStr = "Func. soma: entrada:" ++ show soma1Input ++ "; resultado:" ++ show soma1Resultado
putStrLn soma1TesteStr


--sempre teste
let sempreInput = "6"
let sempreResultado = sempre sempreInput
let sempreTesteStr = "Func. sempre: entrada:" ++ show sempreInput ++ "; resultado:" ++ show sempreResultado
putStrLn sempreTesteStr

--treco teste
let trecoInput1 = 1.1
let trecoInput2 = 1.2
let trecoInput3 = 3.4
let trecoResultado = treco trecoInput1 trecoInput2 trecoInput3
let trecoTesteStr = "Func. treco: entrada:" ++ show trecoInput1 ++ ", " ++ show       trecoInput2 ++ ", " ++ show trecoInput3 ++ "; resultado: " ++ show trecoResultado
putStrLn trecoTesteStr

--resto teste
let restoInput1 = 7
let restoInput2 = 2
let restoResultado = resto restoInput1 restoInput2
let restoTesteStr = "Func. resto: entrada:" ++ show restoInput1 ++ ", " ++ show restoInput2 ++ "; resultado:" ++ show restoResultado
putStrLn restoTesteStr

let restoInput1 = 7
let restoInput2 = 7
let restoResultado = resto restoInput1 restoInput2
let restoTesteStr = "Func. resto: entrada:" ++ show restoInput1 ++ ", " ++ show restoInput2 ++ "; resultado:" ++ show restoResultado
putStrLn restoTesteStr

--precoMaior teste
let precoMaiorInput1 = 4.55
let precoMaiorInput2 = 7.89
let precoMaiorInput3 = 5.50
let precoMaiorInput4 = 2.33
let precoMaiorResultado = precoMaior precoMaiorInput1 precoMaiorInput2 precoMaiorInput3 precoMaiorInput4
let precoMaiorTesteStr = "Func. precoMaior: entrada:" ++ show precoMaiorInput1 ++ ", " ++ show precoMaiorInput2 ++ ", " ++ show precoMaiorInput3 ++ ", " ++ show precoMaiorInput4 ++ "; resultado:" ++ show precoMaiorResultado
putStrLn precoMaiorTesteStr

--impar teste
let imparInput1 = 3
let imparInput2 = 5
let imparResultado = impar imparInput1 imparInput2  --Impar
let imparTesteStr = "Func. impar: entrada:" ++ show imparInput1 ++ ", " ++ show imparInput2 ++ "; resultado:" ++ show imparResultado 
putStrLn imparTesteStr

let imparInput1 = 3
let imparInput2 = 2
let imparResultado = impar imparInput1 imparInput2   --Par
let imparTesteStr = "Func. impar: entrada:" ++ show imparInput1 ++ ", " ++ show imparInput2 ++ "; resultado:" ++ show imparResultado 
putStrLn imparTesteStr

--somaPar teste
let somaParInput = (10,5)
let somaParResultado = somaPar somaParInput
let somaParTesteStr = "Func. somaPar: entrada:" ++ show somaParInput ++ "; resultado:" ++ show somaParResultado 
putStrLn somaParTesteStr

--equação 𝑥2 +𝑦2 +𝑧 teste
let equacaoInput1 = 2.0
let equacaoInput2 = 3.5
let equacaoInput3 = 4.4
let equacaoResultado = equacao equacaoInput1 equacaoInput2 equacaoInput3
let equacaoTesteStr = "Func. equação: entrada:" ++ show equacaoInput1 ++ ", " ++ show equacaoInput2 ++ ", " ++ show equacaoInput3 ++ "; resultado:" ++ show equacaoResultado 
putStrLn equacaoTesteStr

--indiceIMC teste
let indiceIMCInput1 = 63.0
let indiceIMCInput2 = 1.86
let indiceIMCResultado = indiceIMC indiceIMCInput1 indiceIMCInput2
let indiceImcTesteStr = "Func. indiceIMC: entrada:" ++ show indiceIMCInput1 ++ ", " ++ show indiceIMCInput2 ++ "; resultado:"  ++ show indiceIMCResultado
putStrLn indiceImcTesteStr

--bissexto teste
let bissextoInput = 2000 --É bissexto
let bissextoResultado = bissexto bissextoInput
let bissextoTesteStr = "Func. bissexto: entrada:" ++ show bissextoInput ++ "; resultado:"  ++ show bissextoResultado
putStrLn bissextoTesteStr

let bissextoInput = 1997  --Não é bissexto
let bissextoResultado = bissexto bissextoInput
let bissextoTesteStr = "Func. bissexto: entrada:" ++ show bissextoInput ++ "; resultado:"  ++ show bissextoResultado
putStrLn bissextoTesteStr