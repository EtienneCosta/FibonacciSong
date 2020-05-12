module Music where
import Data.Char
import Data.List
import Data.Bits
import Cp
import List
import Nat
import Euterpea
{- 
Função para leitura de valores: 

Parâmetro : Recebe uma String como parâmetro.
putStrLn : Escreve uma string e o newline para o standard output.
getLine : Permite capturar o conteúdo escrito.

-}


{-
Autor da sequência de fibonacci	William Lee Irwin III 
https://mail.haskell.org/pipermail/haskell-cafe/2005-January/008839.html

-}
fibonacci :: Int -> Integer
fibonacci n = snd . foldl' fib (1, 0) . dropWhile not $
            [testBit n k | k <- let s = bitSize n in [s-1,s-2..0]]
    where
        fib (f, g) p
            | p         = (f*(f+2*g), ss)
            | otherwise = (ss, g*(2*f-g))
            where ss = f*f+g*g

{-
Converte uma lista de valores em uma String.
ex: f [1,2,3,4] = "1234"
-}
intToDigit'::[Integer]->String
intToDigit' = cataList (either nil ((conc.(show><id))))


{-
Calcula o valor de fibonacci para um conjunto de valores do tipo Inteiro.
-}

fibRange'::[Int]->[Integer]
fibRange' = cataList (either nil (cons.(fibonacci><id)))


{-
Converte uma String em notas musicais
-}

{-
E Major Scale : 
The notes of the the scale are E, F♯, G♯, A, B, C♯, and D♯. 
The note, E repeats one octave higher. Its key signature has four sharps.
-}
makeNote::Char->Music Pitch
makeNote x | x=='0'= ef 4 qn
           | x=='1'= e 4 qn
           | x=='2'= fs 4 qn
           | x=='3'= gs 4 qn
           | x=='4'= a 4 qn
           | x=='5'= b 4 qn
           | x=='6'= cs 4 qn
           | x=='7'= ds 4 qn
           | x=='8'= e 4 qn -- Provavelmente existe a necessidade de aplicar o transpose ou encontrar está nota em particular
           | otherwise = fs 4 qn -- Provavelmente existe a necessidade de aplicar o transpose ou encontrar está nota em particular

charToMusic::String->[Music Pitch]
charToMusic = cataList(either nil (cons.(makeNote><id)))



{-
Sequência Final:
1º Para um dado conjunto de valores calcular a sequência de fibonacci: fibRange'
2º Para uma lista de valores converter para String: intToDigit'
3º Converter uma String para uma lista de Notas Musicais.
4º Play
-}
playFibonacci::[Int]->IO()
playFibonacci l  = play (line (charToMusic(intToDigit'(fibRange' l))))


{-
Twinkle Twinkle Little Star Example
-}
twinkle =play $ line [c 4 qn, c 4 qn, g 4 qn, g 4 qn, a 4 qn, a 4 qn, g 4 hn]




main::IO()
main = do  
           putStrLn "Indique o limite superior da sequência de Fibonacci."
           number <- getLine
           let value = read number
           playFibonacci [1..value]
           print $ "Life is better with music :) "
           


           
