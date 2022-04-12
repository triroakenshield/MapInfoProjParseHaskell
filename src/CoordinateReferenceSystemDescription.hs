{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module CoordinateReferenceSystemDescription where

import Data.Either
import qualified Data.Text as T

import Ellipsoid
import Datum
import DatumExt
import Projection ( Projection(Projection), getProjectionById )

data CoordinateReferenceSystemDescription = CoordinateReferenceSystemDescription {
    name :: T.Text,
    projection :: Projection
}

sWord :: T.Text
sWord = T.pack "\""

list12 :: T.Text -> (T.Text, T.Text)
list12 = T.breakOnEnd sWord

getName :: T.Text -> T.Text
getName = fst . list12

getTail :: T.Text -> T.Text
getTail = snd . list12

sep :: T.Text
sep = T.pack ","

getParameters :: T.Text -> [T.Text]
getParameters = T.splitOn sep . getTail

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

getParameterId :: T.Text -> Integer
getParameterId = getInteger . getParameterStr 1

getNextParameterStr :: [T.Text] -> (T.Text, [T.Text])
getNextParameterStr list = (head list, tail list)

getNextParameterInteger :: [T.Text] -> (Integer, [T.Text])
getNextParameterInteger list = (getInteger (head list), tail list)

getNextParameterDouble :: [T.Text] -> (Double, [T.Text])
getNextParameterDouble list = (getDouble (head list), tail list)

testProjectionId :: Integer -> Integer
testProjectionId res  
             | res > 2999 = res - 3000 
             | res > 1999 = res - 2000 
             | res > 999  = res - 1000
             | otherwise = res

getProjection :: T.Text -> Projection
getProjection = getProjectionById . testProjectionId . getParameterId

getDatumFromList :: (Integer, [T.Text]) -> Datum
getDatumFromList (int1 , list1) = Datum.Datum int1 (-1) "" "userDef" (getEllipsoidById (getIntegerN list1 0)) (getDoubleN list1 1) (getDoubleN list1 2) (getDoubleN list1 3)


getDatumExtFromList (int1 , list1) = DatumExt.DatumExt int1 (-1) "" "userDef" (getEllipsoidById (getIntegerN list1 0)) (getDoubleN list1 1) (getDoubleN list1 2) (getDoubleN list1 3) (getDoubleN list1 4) (getDoubleN list1 5) (getDoubleN list1 6) (getDoubleN list1 7) (getDoubleN list1 8)
--data DatumDesc = Left Datum | Right DatumExt

getDatum (int1 , list1)
    | int1 < 999 = Left (Datum.getDatumById int1)
    | int1 == 999 = Left (getDatumFromList (int1 , list1))
    | int1 < 9999 =  Right (DatumExt.getDatumExtById int1)
    | int1 == 9999 =   Right (getDatumExtFromList (int1 , list1))
    | otherwise = Left (Datum.getDatumById 1) -- TODO Тут должна быть ошибка!!