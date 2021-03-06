module Description where

import qualified Data.Text as T

sWord :: T.Text
sWord = T.pack "\""

splitString :: T.Text -> (T.Text, T.Text)
splitString = T.breakOnEnd sWord

getName :: T.Text -> T.Text
getName = fst . splitString

getTail :: T.Text -> T.Text
getTail = snd . splitString

sep :: T.Text
sep = T.pack ","

getParameters :: T.Text -> [T.Text]
getParameters = tail . T.splitOn sep . getTail

getParameterStr :: Int -> T.Text -> T.Text
getParameterStr id text = getParameters text !! id

getInteger ::  T.Text -> Integer
getInteger x = read (T.unpack x) :: Integer

getIntegerN :: [T.Text] -> Int -> Integer
getIntegerN list i = getInteger (list !! i)

getDouble ::  T.Text -> Double
getDouble x = read (T.unpack x) :: Double

getDoubleN :: [T.Text] -> Int -> Double
getDoubleN list i = getDouble (list !! i)

getParameterInt :: T.Text -> Integer
getParameterInt = getInteger . getParameterStr 0

getNextParameterStr :: [T.Text] -> (T.Text, [T.Text])
getNextParameterStr list = (head list, tail list)

getNextParameterInteger :: [T.Text] -> (Integer, [T.Text])
getNextParameterInteger list = (getInteger (head list), tail list)

getNextParameterDouble :: [T.Text] -> (Double, [T.Text])
getNextParameterDouble list = (getDouble (head list), tail list)

filterList :: (a -> t -> Bool) -> [a] -> t -> [a]
filterList func list i = filter (`func` i) list

getItemById :: (a -> t -> Bool) -> [a] -> t -> a
getItemById func list i = head (filterList func list i)

getParamTail :: (Eq t, Num t) => [a] -> t -> [a]
getParamTail list1 i = if i == 0
                       then tail list1
                       else getParamTail (tail list1) (i - 1)