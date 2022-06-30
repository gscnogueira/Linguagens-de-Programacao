{-
Aluno      : Gabriel da Silva Corvino Nogueira
Matrícula  : 18/0113330
Disciplina : Linguagens de Programação – 2022.1
Turma      : B
Professor  : Prof. Vander Alves
-}

import Data.List
-- Questão 1
maior4 :: Int -> Int -> Int -> Int -> Int

maior4 a b c d
  | (a>=b) && (a>=c) && (a>=d) = a
  | (b>=a) && (b>=c) && (b>=d) = b
  | (c>=b) && (c>=a) && (c>=d) = c
  | (d>=b) && (d>=c) && (d>=a) = d

-- Questão 2
converterNotaParaMencao :: Float -> String
converterNotaParaMencao nota
  | (nota>=9) && (nota<=10)     = "SS"
  | (nota>=7) && (nota<=8.9)    = "MS"
  | (nota>=5) && (nota<=6.9)    = "MM"
  | (nota>=3) && (nota<=4.9)    = "MI"
  | (nota>=0.1) && (nota<=2.9)  = "II"
  | nota == 0                   = "SR"

-- Questão 3
isDecrescente :: [Int] -> Bool
isDecrescente [] = True
isDecrescente [_] = True
isDecrescente (a:b:as)
  | a <= b = False
  | (a > b) && (as == []) = True
  | otherwise = isDecrescente (b:as)

-- Questão 4

histograma :: [String] -> [(String, Int)]
histograma [] = []
histograma (a:as) = updateHistograma (histograma as) a
  where
    updateHistograma [] s = [(s, 1)]
    updateHistograma ((word, count):hs) s
      | word == s = (word, count+1):hs
      | otherwise = (word, count):(updateHistograma hs s)

-- Questão 5

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f  [] _= []
myZipWith f  _ [] = []
myZipWith f (a:as) (b:bs)= (f a b):(myZipWith f as bs)

-- Questão 6
aprovadosOrdemDeMedia ::
  [(String,Float,Float)] -> [(String,Float)]

aprovadosOrdemDeMedia [] = []
aprovadosOrdemDeMedia ((aluno, p1, p2):alunos)
  | media >= 5 = sortOn snd ((aluno, media):(aprovadosOrdemDeMedia alunos))
  | otherwise = aprovadosOrdemDeMedia alunos
  where media = (p1 + p2)/2
  

-- Questão 7
--- a)
somaMatricial :: Num u => [[u]] -> [[u]] -> [[u]]
somaMatricial [] [] = []
somaMatricial (a:as) (b:bs)  = (zipWith (+) a b):( somaMatricial bs as)

--- b)
matrizTransposta :: Num u => [[u]] -> [[u]]
matrizTransposta [] = []
matrizTransposta m = (column):(matrizTransposta rest)
  where
    column = [a | (a:as)<-m]
    rest   = [as | (a:as)<-m, (length as)/=0]

--- c)

multiplicaLinha _ [] = []
multiplicaLinha  l  m = 
  sum(zipWith (*) l column):(multiplicaLinha l rest)
  where column = [e  | (e:es)<-m]
        rest   = [es | (e:es)<-m, (length es)/=0]

multiplicacaoMatricial :: Num u => [[u]] -> [[u]] -> [[u]]
multiplicacaoMatricial [] _ = []
multiplicacaoMatricial (a:as) b =
  ( multiplicaLinha a b ):(multiplicacaoMatricial as b)



