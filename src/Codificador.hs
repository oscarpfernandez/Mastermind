module Codificador where

import Fun_aux
import Descodificador

{-esta é a função principal deste módulo-}
codificador :: IO()
codificador = do {putStr "\nJogar com números(n) ou com cores(c)"; p <- getChar;
                  (escolher (toUpper p))
                 }

escolher :: Char -> IO ()
escolher x
 |x=='C' = do {putStr "Cores:\n(a)marelo\n(b)ranco\n(e)ncarnado\n(p)reto\n(v)erde\n(c)astanho\n";
               input_dados2 1 []}
 |x=='N' = do {input_dados 1 []}
 |otherwise = do {putStr "\nEscreva 'c' ou 'n' !\n"; codificador} 


{- contêm as chaves de greenwell-}
chave_estatica :: [[Int]]
chave_estatica = [[1,2,2,1],[2,3,5,4],[3,3,1,1],[4,5,2,4],[5,6,5,6],[6,6,4,3]]


{- é a função que vai fazer a interação com o utilizador, através de números -} 
input_dados :: Int -> [(Int,Int)] -> IO()
input_dados n dados 
 |n/=1 && (head dados)==(4,0) = do {putStr "Acertei!!!";
                                    appendFile historial ("@CNhipoteses$" ++ (show (zip chave_estatica 
                                    (reverse(dados)))) ++ "&G\n")}
 |n<=6 =  do {putStr " \nA solução e?\n" ;
             print (chave_estatica !! (n-1)) ;
             putStr "Numeros correctos na posição correcta?" ;
             x<-getChar ;
             putStr "\n" ;
             putStr "Numeros correctos na posição errada?" ;
             y<-getChar ;
             putStr "\n" ;
             (input_dados (n+1) ((digitToInt x,digitToInt y):dados))}
 |otherwise = do {appendFile historial ("@CNhipoteses$" ++ (show (zip chave_estatica (reverse(dados)))) );
                 putStr "\n"; (descobrir_codigo (reverse dados) 0 )
                 }


{-Esta Função vai comparar os dados introduzidos pelo utilizador com a chave estática e fornece o resultado em numeros-}  
descobrir_codigo :: [(Int,Int)] -> Int -> IO()
descobrir_codigo info plc
 |plc>=1296 = do {putStr "Enganou-se a introduzir os dados!!!\n"} 
 |(lista_result  plc) == (info) = do {putStr "O codigo é \n"; print ((combinacoes ) !! plc);
                                      appendFile historial ( " SF" ++ (show ((combinacoes ) !! plc)) ++ "&G\n" ) }
 |otherwise = descobrir_codigo info (plc +1)  


-- -- -- --


{- é a função que vai fazer a interação com o utilizador, através de cores -} 
input_dados2 :: Int -> [(Int,Int)] -> IO()
input_dados2 n dados 
 |n/=1 && (head dados)==(4,0) = do {putStr "Acertei!!!";
                                    appendFile historial ("@Chipoteses$" ++ (show (zip chave_estatica 
                                    (reverse(dados)))) ++ "&G\n")}
 |n<=6 =  do {putStr " \nA solução e?\n" ;
             print (num_to_cores(chave_estatica !! (n-1))) ;
             putStr "Numero de cores correctas na posição correcta?" ;
             x<-getChar ;
             putStr "\n" ;
             putStr "Numero de cores correctas na posição errada?" ;
             y<-getChar ;
             putStr "\n" ;
             (input_dados2 (n+1) ((digitToInt x,digitToInt y):dados))}
 |otherwise = do {appendFile historial ("@CNhipoteses$" ++ (show (zip chave_estatica (reverse(dados)))) );
                 putStr "\n"; (descobrir_codigo2 (reverse dados) 0 )
                 }

{-Esta Função vai comparar os dados introduzidos pelo utilizador com a chave estática, e fornece o resultado em cores-}  
descobrir_codigo2 :: [(Int,Int)] -> Int -> IO()
descobrir_codigo2 info plc
 |plc>=1296 = do {putStr "Enganou-se a introduzir os dados!!!\n"} 
 |(lista_result  plc) == (info) = do {putStr "O codigo é \n"; print (num_to_cores((combinacoes ) !! plc));
                                      appendFile historial ( " SF" ++ (show ((combinacoes ) !! plc)) ++ "&G\n" )}
 |otherwise = descobrir_codigo2 info (plc +1)  


{- nota : plc <=> (p)osição na (l)ista de (c)ombinações ; pc <=> (p)osição na lista (c)have estatica -}


{-forma a lista de pares ordenados, que resultam da comparação dos inputs do utilizador, com a chave estática-} 
lista_result  plc = [testar_hip ((combinacoes ) !! plc) (chave_estatica !! pc) | pc <- [0 .. 5] ] 
 
{-faz a comparação referida anteriormente-}
testar_hip :: [Int] -> [Int] -> (Int,Int)
testar_hip x y = (testar1 x y , testar2 x y)

testar1 :: [Int] -> [Int] -> Int
testar1 x y =  (length [(a,b) | (a,b)<- (zip x y), a==b])

testar2 :: [Int] -> [Int] -> Int
testar2 x y = length (aux_deco2 (iSort x) (iSort y ))-(testar1 x y)

{- forma a lista das 1296 combinações de códigos possiveis, que se podem obter com os 6 digitos-} 
combinacoes ::  [[Int]]
combinacoes = [[a,b,c,d] | a<-[1,2,3,4,5,6],b<-[1,2,3,4,5,6],c<-[1,2,3,4,5,6],d<-[1,2,3,4,5,6]]


{-Trabalho relizado por Oscar Lopes / 16233 -}
