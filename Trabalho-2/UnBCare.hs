module UnBCare where

import ModeloDados
import Data.List

{-
 *** Aluno: Gabriel da Silva Corvino Nogueira
 *** Matricula: 180113330
 

██╗░░░██╗███╗░░██╗██████╗░  ░█████╗░░█████╗░██████╗░██████╗
██║░░░██║████╗░██║██╔══██╗  ██╔══██╗██╔══██╗██╔══██╗██╔════╝
██║░░░██║██╔██╗██║██████╦╝  ██║░░╚═╝███████║██████╔╝█████╗░░
██║░░░██║██║╚████║██╔══██╗  ██║░░██╗██╔══██║██╔══██╗██╔══╝░░
╚██████╔╝██║░╚███║██████╦╝  ╚█████╔╝██║░░██║██║░░██║███████╗
░╚═════╝░╚═╝░░╚══╝╚═════╝░  ░╚════╝░╚═╝░░╚═╝╚═╝░░╚═╝╚══════╝

O objetivo desse trabalho é fornecer apoio ao gerenciamento de cuidados a serem prestados a um paciente.
O paciente tem um receituario médico, que indica os medicamentos a serem tomados com seus respectivos horários durante um dia.
Esse receituário é organizado em um plano de medicamentos que estabelece, por horário, quais são os remédios a serem
tomados. Cada medicamento tem um nome e uma quantidade de comprimidos que deve ser ministrada.
Um cuidador de plantão é responsável por ministrar os cuidados ao paciente, seja ministrar medicamento, seja comprar medicamento.
Eventualmente, o cuidador precisará comprar medicamentos para cumprir o plano.
O modelo de dados do problema (definições de tipo) está disponível no arquivo Modelo/ModeloDados.hs
Defina funções que simulem o comportamento descrito acima e que estejam de acordo com o referido
modelo de dados.

-}

{-

   QUESTÃO 1, VALOR: 1,0 ponto

Defina a função "comprarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento, uma quantidade e um
estoque inicial de medicamentos, retorne um novo estoque de medicamentos contendo o medicamento adicionado da referida
quantidade. Se o medicamento já existir na lista de medicamentos, então a sua quantidade deve ser atualizada no novo estoque.
Caso o remédio ainda não exista no estoque, o novo estoque a ser retornado deve ter o remédio e sua quantidade como cabeça.

-}

comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento m q [] = [(m, q)]
comprarMedicamento m q e 
  | m == m' = e' ++[(m, q+q')]
  | otherwise = (comprarMedicamento m q e')++[last e]
  where (m', q') = (last e)
        e' = (init e)



{-
   QUESTÃO 2, VALOR: 1,0 ponto

Defina a função "tomarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de medicamentos,
retorna um novo estoque de medicamentos, resultante de 1 comprimido do medicamento ser ministrado ao paciente.
Se o medicamento não existir no estoque, Nothing deve ser retornado. Caso contrário, deve se retornar Just v,
onde v é o novo estoque.

-}

tomarMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
tomarMedicamento m [(m', q')]
  | m == m' && q'> 0 = Just [(m', q'-1)]
  |otherwise = Nothing
tomarMedicamento m ((m', q'):es) 
  | m == m' && q'>0  = Just ((m',(q'-1)):es)
  | otherwise = getIt (m', q') (tomarMedicamento m es)
  where getIt a (Just e) = Just (a:e)
        getIt a Nothing = Nothing
    

{-
   QUESTÃO 3  VALOR: 1,0 ponto

Defina a função "consultarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de
medicamentos, retorne a quantidade desse medicamento no estoque.
Se o medicamento não existir, retorne 0.

-}

consultarMedicamento :: Medicamento -> EstoqueMedicamentos -> Quantidade
consultarMedicamento m [] = 0
consultarMedicamento m ((m',q'):es)
  | m == m' = q'
  | otherwise = consultarMedicamento m es


{-
   QUESTÃO 4  VALOR: 1,0 ponto

  Defina a função "demandaMedicamentos", cujo tipo é dado abaixo e que computa a demanda de todos os medicamentos
  por um dia a partir do receituario. O retorno é do tipo EstoqueMedicamentos e deve ser ordenado lexicograficamente
  pelo nome do medicamento.

  Dica: Observe que o receituario lista cada remédio e os horários em que ele deve ser tomado no dia.
  Assim, a demanda de cada remédio já está latente no receituario, bastando contar a quantidade de vezes que cada remédio
  é tomado.

-}

demandaMedicamentos :: Receituario -> EstoqueMedicamentos
demandaMedicamentos r = sort [(med, (length horarios) ) | (med, horarios) <- r]

{-
   QUESTÃO 5  VALOR: 1,0 ponto, sendo 0,5 para cada função.

 Um receituário é válido se, e somente se, todo os medicamentos são distintos e estão ordenados lexicograficamente e,
 para cada medicamento, seus horários também estão ordenados e são distintos.

 Inversamente, um plano de medicamentos é válido se, e somente se, todos seus horários também estão ordenados e são distintos,
 e para cada horário, os medicamentos são distintos e são ordenados lexicograficamente.

 Defina as funções "receituarioValido" e "planoValido" que verifiquem as propriedades acima e cujos tipos são dados abaixo:

 -}

uniqueAndSorted :: (Eq a, Ord a) => [a] -> Bool
uniqueAndSorted [] = True
uniqueAndSorted [_] = True
uniqueAndSorted (a:b:as)
  | elem a (b:as) = False
  | a > b = False
  | otherwise =  uniqueAndSorted (b:as)

receituarioValido :: Receituario -> Bool
receituarioValido r
  | not (uniqueAndSorted meds) = False
  | any (==False) (map uniqueAndSorted horarios) = False
  | otherwise = True
  where meds     = map fst r
        horarios = map snd r

planoValido :: PlanoMedicamento -> Bool
planoValido p
  | not (uniqueAndSorted horarios) = False
  | any (==False) (map uniqueAndSorted meds) = False
  | otherwise = True
  where horarios = map fst p
        meds     = map snd p

{-

   QUESTÃO 6  VALOR: 1,0 ponto,

 Um plantão é válido se, e somente se, todas as seguintes condições são satisfeitas:

 1. Os horários da lista são distintos e estão em ordem crescente;
 2. Não há, em um mesmo horário, ocorrência de compra e medicagem de um mesmo medicamento (e.g. `[Comprar m1, Medicar m1 x]`);
 3. Para cada horário, as ocorrências de Medicar estão ordenadas lexicograficamente.

 Defina a função "plantaoValido" que verifica as propriedades acima e cujo tipo é dado abaixo:

 -}

sorted :: (Ord a) => [a] -> Bool
sorted [] = True
sorted [_] = True
sorted (a:b:as)
  | a > b = False
  | otherwise = sorted (b:as)

instance Eq Cuidado where
  (Comprar m q) == (Comprar m' q') = m == m'
  (Medicar m)  == (Medicar m') = m==m'
  _ == _ = False

instance Ord Cuidado where
  (Medicar m) > (Medicar m') = m>m'
  (Medicar m) <= (Medicar m') = m<=m'
  
  
verificaCuidados :: [Cuidado] -> Bool
verificaCuidados [] = True
verificaCuidados ((Comprar m q):ms)
  | (Medicar m) `elem` ms = False
  | otherwise = verificaCuidados ms
verificaCuidados ((Medicar m):ms)
  | (Comprar m 0) `elem` ms = False
  | otherwise = verificaCuidados ms


plantaoValido :: Plantao -> Bool
plantaoValido p
  | not (uniqueAndSorted horarios) = False
  | any (==False) (map verificaCuidados cuidados) = False
  | any (==False) (map sorted medicagens) = False
  |otherwise = True
  where horarios = map fst p
        cuidados = map snd p
        medicagens = map (filter isMedicar) cuidados
          where isMedicar (Medicar _) = True
                isMedicar _ = False
{-
   QUESTÃO 7  VALOR: 1,0 ponto

  Defina a função "geraPlanoReceituario", cujo tipo é dado abaixo e que, a partir de um receituario válido,
  retorne um plano de medicamento válido.

  Dica: enquanto o receituário lista os horários que cada remédio deve ser tomado, o plano de medicamentos  é uma
  disposição ordenada por horário de todos os remédios que devem ser tomados pelo paciente em um certo horário.

-}


geraAB :: (Eq a, Ord a, Eq b, Ord b) =>[(a,[b])] -> [(b, [a])]
geraAB x = map getValues keys
  where getValues k = (k , map fst $ filter ((elem k).snd) x)
        keys = sort $ nub $ concat $ map snd x

geraPlanoReceituario :: Receituario -> PlanoMedicamento
geraPlanoReceituario = geraAB


{- QUESTÃO 8  VALOR: 1,0 ponto

 Defina a função "geraReceituarioPlano", cujo tipo é dado abaixo e que retorna um receituário válido a partir de um
 plano de medicamentos válido.
 Dica: Existe alguma relação de simetria entre o receituário e o plano de medicamentos? Caso exista, essa simetria permite
 compararmos a função geraReceituarioPlano com a função geraPlanoReceituario ? Em outras palavras, podemos definir
 geraReceituarioPlano com base em geraPlanoReceituario ?

-}

geraReceituarioPlano :: PlanoMedicamento -> Receituario
geraReceituarioPlano = geraAB

{-  QUESTÃO 9 VALOR: 1,0 ponto

Defina a função "executaPlantao", cujo tipo é dado abaixo e que executa um plantão válido a partir de um estoque de medicamentos,
resultando em novo estoque. A execução consiste em desempenhar, sequencialmente, todos os cuidados para cada horário do plantão.
Caso o estoque acabe antes de terminar a execução do plantão, o resultado da função deve ser Nothing. Caso contrário, o resultado
deve ser Just v, onde v é o valor final do estoque de medicamentos

-}

executaCuidado :: Cuidado -> Maybe EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executaCuidado (Comprar m q) (Just e) = Just $ comprarMedicamento m q e
executaCuidado (Medicar m) (Just e) = tomarMedicamento m  e

executaCuidados :: [Cuidado] -> Maybe EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executaCuidados _ Nothing = Nothing
executaCuidados [] e = e
executaCuidados (a:as) e = executaCuidados as (executaCuidado a e)

executaPlantao :: Plantao -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executaPlantao  p e = executaCuidados cuidados (Just e)
  where cuidados = concat $ map snd p

{-
QUESTÃO 10 VALOR: 1,0 ponto

Defina uma função "satisfaz", cujo tipo é dado abaixo e que verifica se um plantão válido satisfaz um plano
de medicamento válido para um certo estoque, ou seja, a função "satisfaz" deve verificar se a execução do plantão
implica terminar com estoque diferente de Nothing e administrar os medicamentos prescritos no plano.
Dica: fazer correspondencia entre os remédios previstos no plano e os ministrados pela execução do plantão.
Note que alguns cuidados podem ser comprar medicamento e que eles podem ocorrer sozinhos em certo horário ou
juntamente com ministrar medicamento.

-}


satisfaz :: Plantao -> PlanoMedicamento -> EstoqueMedicamentos -> Bool
satisfaz plantao plano estoque
  | (executaPlantao plantao estoque) == Nothing = False
  | (horarios plantao) /= (horarios plano) = False
  | any (==False) (zipWith belongsTo medsPlantao medsPlano) = False
  | otherwise = True
  where horarios = map fst
        isMedicar (Medicar m) = True
        isMedicar _ = False
        medsPlantao = map ((map (\(Medicar a) -> a)).(filter isMedicar).(snd)) plantao
        medsPlano = map snd plano
        belongsTo a b =  intersect a b == a


{-

QUESTÃO 11 VALOR: 1,0 ponto

 Defina a função "plantaoCorreto", cujo tipo é dado abaixo e que gera um plantão válido que satisfaz um plano de
 medicamentos válido e um estoque de medicamentos.
 Dica: a execução do plantão deve atender ao plano de medicamentos e ao estoque.

-}
quantidadeComprar estoque (a,b)
  | sobra < 0  = (a,-sobra)
  | otherwise = (a, 0)
  where sobra = (consultarMedicamento a estoque) - (length b)

plantaoCorreto :: PlanoMedicamento -> EstoqueMedicamentos -> Plantao
plantaoCorreto plano estoque = addCompras (map (\(a,b)->(a, map Medicar b)) plano) compras
  where quantidades = map (quantidadeComprar estoque) (geraReceituarioPlano plano)
        compras = map (uncurry Comprar) (filter ((/=0).(snd)) quantidades)
        h = (fst $ head plano) - 1
        addCompras ((a, b):abs) c = ((a, b++c):abs)

