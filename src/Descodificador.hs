module Descodificador where 

import Fun_aux
import Random


aleatorio :: Int -> Int -> (Int,Int) -> [Int]
aleatorio n semente (min,max) = take n (randomRs (min,max) (mkStdGen semente))

code :: Int -> [Int]
code k = aleatorio 4 k (1,6)

{-realiza a intersecção de duas listas-}
aux_deco2 :: [Int] -> [Int] -> [Int]
aux_deco2 [] _ = []
aux_deco2 _ [] = []
aux_deco2 (x:xs) w
 |(elem x w) = x:(aux_deco2 xs (retirar_elem x w))
 |otherwise = (aux_deco2 xs w)

{-dado um determinado inteiro e uma lista, a função procura esse inteiro e retira-o da lista -}
retirar_elem :: Int -> [Int] -> [Int]
retirar_elem x [] = []
retirar_elem x (z:r)
 |x==z = r
 |otherwise = z:(retirar_elem x r)



descodificador = do {putStr "\nJogar com números(n) ou com cores(c)"; p <- getChar;
                     (escolher2 (toUpper p))
                    }

escolher2 :: Char -> IO ()
escolher2 x
 |x=='N' = do {putStr " \nIntroduza um numero natural "; 
               x <- getLine; 
               putStr " \nINSTRUÇÕES: Introduza 4 numeros de 1 a 6\n Por exemplo 1234\n";
               putStr "Introduza uma hipotese\n"; 
               l <- getLine;
               appendFile historial ("@DN" ++ "hipoteses#" ++ (show l) );
               (testar l 1 (stringToInt x))
               }
               
 |x=='C' = do {putStr "\nIntroduza um numero natural "; 
               x <- getLine; 
               putStr "Cores:\n(a)marelo\n(b)ranco\n(e)ncarnado\n(p)reto\n(v)erde\n(c)astanho\n";
               putStr "Introduza uma hipotese\n" ; 
               l <- getLine;
               appendFile historial ("@DN" ++ "hipoteses#" ++ (show (cores_to_num l)) );
               (testar3 (  l) 1 (stringToInt x))
              }

 |otherwise = do {putStr "\nEscreva 'c' ou 'n' !\n"; descodificador} 



{-esta função avalia as hipoteses numericas introduzidas-}
testar :: String -> Int -> Int -> IO()
testar s n k 
 |(decoder s k n)==(4,0) = do {appendFile historial ((show (decoder s k n)) ++ "%" ++ "W" ++
                               (show (n-1)) ++ " \n");
                               putStr "Ganhou!!!! Derrotou o Mastermind!!!"}
 |(decoder s k n)/=(4,0) && n<10= do {print (decoder s k n);
                                    putStr "Introduza outra hipotese\n";
                                    l<-getLine;
                                    appendFile historial ((show (decoder s k n)) ++ (show l) );
                                    testar l (n+1) k
                                    }                                    
 |otherwise          = do {putStr "A solução é "; (print (code k)); 
                           putStr "Acabaram as jogadas!\n Tente de novo!!!";
                           appendFile historial (" SF" ++ (show (code k)) ++ "%L\n")
                           }

{-esta função avalia as hipoteses de cores introduzidas-}
testar3 :: String -> Int -> Int -> IO()
testar3 s n k 
 |(decoder (cores_to_num s) k n)==(4,0) = do {appendFile historial ((show (decoder (cores_to_num s) k n)) ++ " %" ++
                                              "W" ++ (show (n-1)) ++ "\n");
                                              putStr "Ganhou!!!! Derrotou o Mastermind!!!"}
 |(decoder (cores_to_num s) k n)/=(4,0) && n<10= do {print (decoder (cores_to_num s) k n);
                                    putStr "Introduza outra hipotese\n";
                                    l<-getLine;
                                    appendFile historial ((show (decoder (cores_to_num s) k n)) ++ (show (cores_to_num
                                    l)) );
                                    testar3  l (n+1) k
                                    }                                    
 |otherwise          = do {putStr "A solução é "; (print (num_to_cores(code k))); 
                           putStr "Acabaram as jogadas!\n Tente de novo!!!";
                           appendFile historial (" SF" ++ (show (num_to_cores(code k))) ++ "%L\n")
                           }


{- avalia cada uma das hipoteses individuais -}
decoder :: String -> Int -> Int -> (Int,Int)
decoder hip k n = (decoder1 hip k, decoder2 hip k)

{-fornece o nº de elementos certos na posição certa -}
decoder1 :: String -> Int -> Int 
decoder1 hip k =  (length [(a,b) | (a,b)<- (zip (code k) (lista_num hip)), a==b])

{-fornece o nº de elementos certos na posição errada -}
decoder2 :: String -> Int -> Int
decoder2 hip k = length (aux_deco2 (iSort(code k)) (iSort(lista_num hip)))-(decoder1 hip k)


{-Trabalho relizado por Oscar Lopes / 16233 -}
