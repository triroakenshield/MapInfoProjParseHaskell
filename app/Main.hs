module Main where

import qualified Data.Text as T

import Description
import Unit
import Param (Parameter(Parameter), parameterList, textParameter, showParameter, testParameter, getParameterById)
import Datum
import DatumExt
import Projection  ( Projection(Projection), getProjectionById )
import CoordinateReferenceSystemDescription

aWord :: T.Text
aWord = T.pack "\"МСК-66 зона 1, 3 градусная\", 8, 1001, 7, 60.05, 0, 1, 1500000, -5911057.63"

list1 :: (T.Text, T.Text)
list1 = T.breakOnEnd sWord aWord

list11 :: T.Text -> (T.Text, T.Text)
list11 = T.breakOnEnd sWord

index3 :: Maybe Int
index3 = T.findIndex  (== '"') aWord

thirdWord :: String
thirdWord = (T.unpack . fst) list1

tail1 :: T.Text
tail1 = snd list1

tail11 :: T.Text -> T.Text
tail11 x = snd list1

list2 :: [T.Text]
list2 = T.splitOn sep tail1

list22 :: T.Text -> [T.Text]
list22 = T.splitOn sep . tail11

i2 :: Integer
i2 = read val1
     where val1 = T.unpack elem1
                  where elem1 = list2 !! 1

teest :: Integer -> Integer
teest res
             | res > 2999 = res - 3000
             | res > 1999 = res - 2000
             | res > 999  = res - 1000

test1 = getDatumFromList (999, [" 3"," -150"," -251"," -2"])

testList1 = [" 3", "23.57","-140.95", "-79.8", "0", "-0.35", "-0.79", "-0.22", "1.1"]

test2 = getDatumExtFromList (9999, [" 3", "23.57","-140.95", "-79.8", "0", "-0.35", "-0.79", "-0.22", "1.1"])

test3 = getParamTailByDatum (9999, [" 3", "23.57","-140.95", "-79.8", "0", "-0.35", "-0.79", "-0.22", "1.1"])

test4 = getParamTailByDatum (999, [" 3"," -150"," -251"," -2"])

aWord1 =  T.pack "\"МСК-27 зона 1\", 8, 9999, 3, 23.57, -140.95, -79.8, 0, -0.35, -0.79, -0.22, 0, 7, 130.71666666666, 0, 1, 1300000, -4916586.44"

parse1 text = ( getName text, getParameters text, (getProjection (head (getParameters text)))) -- , (getProjection (head (getParameters text)))

p1 = head (getParameters aWord1)

p2 = getProjection p1

--test6 = test5 p2 (tail (getParameters aWord1))

p3 = parseParam1 (1, (getParameters aWord1))

--list3 = [(getDatumById 3), (getUnitById 7)] -- , (getUnitById 7)

data Contain = CFoo Datum | CBar DatumExt
list4 = [CFoo test1, CBar test2]

crs = getCRS2 aWord1

main :: IO ()
main = (putStrLn . T.unpack) tail1

--main = (putStrLn . showParameter) test
       --where test = getParameterById 1

--main = (putStrLn . T.unpack . showParameter) test1