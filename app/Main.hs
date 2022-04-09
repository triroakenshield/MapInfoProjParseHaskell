module Main where

import qualified Data.Text as T

import Param (Parameter(Parameter), parameterList, textParameter, showParameter, testParameter, getParametersById, getParameterById)

aWord :: T.Text
aWord = T.pack "\"МСК-66 зона 1, 3 градусная\", 8, 1001, 7, 60.05, 0, 1, 1500000, -5911057.63"

sWord :: T.Text
sWord = T.pack "\""

list1 :: (T.Text, T.Text)
list1 = T.breakOnEnd sWord aWord

index3 :: Maybe Int
index3 = T.findIndex  (== '"') aWord

thirdWord :: String
thirdWord = (T.unpack . fst) list1

tail1 :: T.Text
tail1 = snd list1

sep :: T.Text
sep = T.pack ","

list2 :: [T.Text]
list2 = T.splitOn sep tail1

i2 :: Integer
i2 = read val1
     where val1 = T.unpack elem1
                  where elem1 = list2 !! 1

main :: IO ()
main = (putStrLn . showParameter) test
       where test = getParameterById 1

--main = (putStrLn . T.unpack . showParameter) test1