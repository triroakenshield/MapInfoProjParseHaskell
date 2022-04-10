module Description where

getItemById :: (a -> Integer -> Bool) -> [a] -> Integer -> a
getItemById func list i =  head (filter (`func` i) list)