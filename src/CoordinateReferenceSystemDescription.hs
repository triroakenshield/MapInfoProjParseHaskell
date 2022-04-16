{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module CoordinateReferenceSystemDescription where

import Data.Either
import qualified Data.Text as T

import Description

import Ellipsoid
import Datum
import DatumExt
import Projection ( Projection(Projection), getProjectionById )

data CoordinateReferenceSystemDescription = CoordinateReferenceSystemDescription {
    name :: T.Text,
    projection :: Projection
}



testProjectionId :: Integer -> Integer
testProjectionId res  
             | res > 2999 = res - 3000 
             | res > 1999 = res - 2000 
             | res > 999  = res - 1000
             | otherwise = res

getProjection :: T.Text -> Projection
getProjection = getProjectionById . testProjectionId . getParameterInt

getDatumFromList :: (Integer, [T.Text]) -> Datum
getDatumFromList (int1 , list1) = Datum.Datum int1 (-1) "" "userDef" (getEllipsoidById (getIntegerN list1 0)) (getDoubleN list1 1) (getDoubleN list1 2) (getDoubleN list1 3)

getDatumExtFromList :: (Integer, [T.Text]) -> DatumExt
getDatumExtFromList (int1 , list1) = DatumExt.DatumExt int1 (-1) "" "userDef" (getEllipsoidById (getIntegerN list1 0)) (getDoubleN list1 1) (getDoubleN list1 2) (getDoubleN list1 3) (getDoubleN list1 4) (getDoubleN list1 5) (getDoubleN list1 6) (getDoubleN list1 7) (getDoubleN list1 8)
--data DatumDesc = Left Datum | Right DatumExt

getDatum (int1 , list1)
    | int1 < 999 = Left (Datum.getDatumById int1)
    | int1 == 999 = Left (getDatumFromList (int1 , list1))
    | int1 < 9999 =  Right (DatumExt.getDatumExtById int1)
    | int1 == 9999 =   Right (getDatumExtFromList (int1 , list1))
    | otherwise = Left (Datum.getDatumById 1) -- TODO Тут должна быть ошибка!!

getParamTailByDatum (int1 , list1)
    | int1 < 999 = list1
    | int1 == 999 = getParamTail list1 3
    | int1 < 9999 = list1
    | int1 == 9999 = getParamTail list1 8
    | otherwise =  list1