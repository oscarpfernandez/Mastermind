module Fun_aux where 


{-ordena uma lista de inteiros -}
iSort :: [Int] -> [Int]             
iSort [] = []                       
iSort (x:xs) = ins x (iSort xs)       
                                    
ins :: Int -> [Int] -> [Int]        
ins x [] = x:[]                     
ins x (y:ys)                        
 |x <= y = x:(y:ys)                 
 |otherwise = y:(ins x ys)          

{-transforma yuma string numerica num numero inteiro -}
stringToInt :: String -> Int
stringToInt (x:xs)
 |(apenasdigito (x:xs)) = aux (x:xs)
 |otherwise = error "lista não numerica"

apenasdigito :: String -> Bool
apenasdigito [] = True
apenasdigito (x:xs)
 |(isDigit x )==True = (apenasdigito xs)
 |otherwise = False

aux :: String -> Int
aux [] = 0
aux (x:xs) = (digitToInt x)*(10^((length xs))) + (aux xs)

{-transforma uma lista de caracteres numa lista de numeros-}
lista_num :: String -> [Int]
lista_num s = [digitToInt a | a <- s]

{-retira os elementos repetidos de uma lista ordenada -}
retirar_rep :: [Int] -> [Int]
retirar_rep [] = []
retirar_rep [x] = [x]
retirar_rep (x:(y:r))
 |x==y = retirar_rep (y:r)
 |otherwise = x:(retirar_rep (y:r))

-- -- -- 
historial :: FilePath
historial = "hist_MM.txt"

{-função que transforma uma lista númerica numa lista com cores-}
num_to_cores :: [Int] -> String
num_to_cores [] = []
num_to_cores (x:r) = (transf x):(num_to_cores r)

transf :: Int -> Char
transf x 
 |x==1 = 'a'
 |x==2 = 'b'
 |x==3 = 'e'
 |x==4 = 'p'
 |x==5 = 'v'
 |x==6 = 'c'

{-função que transforma uma lista de cores, numa lista de numeros -}
cores_to_num :: String -> [Char]
cores_to_num [] = []
cores_to_num (x:r) = (transf2 x):(cores_to_num r)

transf2 :: Char -> Char
transf2 x
 |x=='a' = '1'
 |x=='b' = '2' 
 |x=='e' = '3'
 |x=='p' = '4'
 |x=='v' = '5'
 |x=='c' = '6'



{-Trabalho relizado por Oscar Lopes / 16233 -}
