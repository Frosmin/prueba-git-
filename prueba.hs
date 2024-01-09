doble x = 2 * x

mayor x y = if x > y then "mayorcito"
    else "menorcito"


currySimon x (a,b) = x


vocales x = case x of 
    'a' -> 'e'
    'e' -> 'i'
    'i' -> 'o'
    'o' -> 'u'
    _ -> 'a'





dijitos:: Int -> String 
dijitos x = case x of 
    1 -> "uno"
    2 -> "dos"



juna:: (Int, Int ) -> Bool
juna (x,y) = case (x,y) of 
    (1,1) -> True
    (1,0) -> False 
    (0,1) -> False
    (0,0) -> False

--Listas






factorial n = if n == 0 then 1
        else n*(factorial (n-1))





sumaListas xs c | c >= (length xs) = 0 
                |otherwise = (xs !! c) + sumaListas xs (c+1)



fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)  


 --0 1 1 2 3 5 8

--una funcion recursiva que haga la potencia de un numero 
potencia n p | p == 0 = 1
             | otherwise = n * potencia n (p-1)

-- numeros primos 
primos n i con
    | i == n = if con > 2 then True else False
    | mod n i == 0 = primos n (i + 1) con
    | otherwise = primos n (i + 1) (con+1)

restaLista xs xy i vacia 
                         | length xs == i = vacia
                         |  xs!!i == xy!!i = restaLista xs xy (i+1) vacia 
                         | otherwise = xs!!i : restaLista xs xy (i+1) vacia



restaLista2 xs xy i vacia
  | length xs == i = vacia
  | xs !! i == xy !! i = restaLista2 xs xy (i + 1) vacia
  | otherwise = xs !! i : restaLista2 xs xy (i + 1) vacia



miReverse [] = []
miReverse (x:xs) =  miReverse xs ++ [x]  

miSum [] = 0
miSum (x:xs) = x + (miSum xs)