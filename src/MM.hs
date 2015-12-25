module MM where 

import Descodificador
import Codificador
import Estatistica
import Fun_aux

-- main: função principal para o jogo Mastermind
main :: IO ()
main = do {putStrLn "Mastermind\nOpções do jogador :\n -Codificador   (c)\n -Descodificador(d)\n -Estatistica   (e)\n -Sair          (s)\n";
	  l <- getChar;
	  opcao (toUpper l);
	  return ();
          }

-- cod_des: de acordo com a opção do jogador, escolhe o modo descodificador ou codificador
opcao :: Char -> IO()
opcao x
 |x=='C' = do codificador 
 |x=='D' = do descodificador
 |x=='E' = do estatistica
 |x=='S' = do return ()
 |otherwise = do {putStr "Escreva 'c', 'd' ou 's'.\n";main}
   
{-Trabalho relizado por Oscar Lopes / 16233 -}
