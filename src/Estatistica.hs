module Estatistica where

import Fun_aux

{-função principal deste módulo-}
estatistica = do {putStr "\nOpções\n-Informações Estatisticas(i)\n-Ver jogos anteriores    (v)\n-Sair                    (s)\n";
                  l <- getChar;
	          opcoes (toUpper l);
	          return ();
                  }

opcoes :: Char -> IO()
opcoes x
 |x=='I' = do info_estatistica 
 |x=='V' = do construir_jogo
 |x=='S' = do return ()
 |otherwise = do {putStr "Escreva 'i', 'v' ou 's'.\n"}

{-permite o cálculo e o output dos dados estatisticos -}
info_estatistica = do {putStr "\n  Estatistica\n -Numero total de jogos efectuados -> ";
                  l <- readFile historial;
                  print (length (filter (=='@') (show l)));
                  putStr " -Numero de jogos ganhos pelo:\n   Codificador    -> ";
                  print (length(encontrar 'G' (show l)));
                  putStr "   Descodificador -> ";
                  print (length(encontrar 'W' (show l)));
                  estat_descod 'W' (show l) 
                  }


estat_descod :: Char -> String -> IO()
estat_descod x l
 |length(encontrar 'W' l) == 0 = do {putStr " -Numero de jogadas do descodificador até acertar:\n   Minimo -> Não existem dados\n   Maximo -> Não existem dados\n   Medio  -> Não existem dados\n"
                                            }
 |otherwise = do {putStr " -Numero de jogadas do descodificador até acertar:\n   Minimo -> "; 
                  print (minimum (encontrar 'W' l));
                  putStr "   Maximo -> ";
                  print (maximum (encontrar 'W' l));
                  putStr "   Medio  -> ";
                  print ( (sum (encontrar 'W' l)) `div` (length(encontrar 'W' l)) ) 
                 }




{-procura numa string, um digito, que se encontra a um dado caracter-}
encontrar :: Char -> String -> [Int]
encontrar a [] = []
encontrar a (l:r)
 |a==l = (digitToInt (head r) +1): (encontrar a r)
 |otherwise  = encontrar a r 



{- reconstrução dos jogos , procurar na string, o jogo  w='@', (a:r) é a string que resulta dos ficheiro txt, ac é um acumulador-}

procurar_jogo :: Int -> Char -> String -> Int -> Int
procurar_jogo 0 w l ac = ac
procurar_jogo x w [] ac = 0
procurar_jogo x w (a:r) ac
 |w==a = procurar_jogo (x-1) w r (ac +1)
 |otherwise = procurar_jogo x w r (ac +1)

{-com base na posição dos marcadores, obtida através da função anterior, esta função lê o txt e constroi a string-}
constr_frase :: Int -> Int -> String -> String
constr_frase x y [] = [] 
constr_frase x y (a:r) 
 |x<y && y<(length (a:r)) = ((a:r) !! x): (constr_frase (x+1) y (a:r))
 |x==y = []


{-função que controla a visualização dos jogos anteriores -}
construir_jogo = do {l<-readFile historial;
                     putStr "\nVer jogos do Codificador(c) ou Descodificador(d)?\n";
                     a<- getChar;
                     ver_jogo (toUpper a) 
                    }  

ver_jogo :: Char -> IO()
ver_jogo x 
 |x=='C' = do {l<-readFile historial; 
               putStr "\nNumero de jogos disponiveis: ";
               print (length(encontrar 'G' (show l))); putStr "Escreva o numero do jogo -> ";
               b<- getChar;putStr "\n";
               putStr(constr_frase (procurar_jogo (digitToInt b) '$' (show l) 0) (procurar_jogo (digitToInt b) '&'(show 
               l) 0) (show l))
              }
 |x=='D' = do {l<-readFile historial; 
               putStr "\nNumero de jogos disponiveis: ";
               print (length(encontrar 'W' (show l)) + length(encontrar 'L' (show l)) ); 
               putStr "Escreva o numero do jogo -> " ;
               b<- getChar;putStr "\n";
               putStr(constr_frase (procurar_jogo (digitToInt b) '#' (show l) 0) (procurar_jogo (digitToInt b) '%' (show 
               l) 0) (show l))
              }
 |otherwise = do {putStr "Escreva 'c' ou 'd'\n"; construir_jogo}


{-Trabalho relizado por Oscar Lopes / 16233 -}
