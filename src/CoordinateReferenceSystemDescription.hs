{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module CoordinateReferenceSystemDescription where

import Data.Either
import qualified Data.Text as T

import Description
import Unit
import Ellipsoid
import Datum
import DatumExt
import Projection ( Projection(Projection), getProjectionById )

data ParamDoble = ParamDoble {
       value :: Double
}

data ParameterValue = DatumDesc Datum | DatumExtDesc DatumExt | UnitDesc Unit | ParamDesc ParamDoble | NothingDesc

data CoordinateReferenceSystemDescription = CoordinateReferenceSystemDescription {
    name :: T.Text,
    projection :: Projection,
    values :: [ParameterValue]
}

testProjectionId :: Integer -> Integer
testProjectionId res  
             | res > 2999 = res - 3000 
             | res > 1999 = res - 2000 
             | res > 999  = res - 1000
             | otherwise = res

getProjection :: T.Text -> Projection
getProjection = getProjectionById . testProjectionId . getInteger

getDatumFromList :: (Integer, [T.Text]) -> Datum
getDatumFromList (int1 , list1) = Datum.Datum int1 (-1) "" "userDef" (getEllipsoidById (getIntegerN list1 0)) (getDoubleN list1 1) (getDoubleN list1 2) (getDoubleN list1 3)

getDatumExtFromList :: (Integer, [T.Text]) -> DatumExt
getDatumExtFromList (int1 , list1) = DatumExt.DatumExt int1 (-1) "" "userDef" (getEllipsoidById (getIntegerN list1 0)) (getDoubleN list1 1) (getDoubleN list1 2) (getDoubleN list1 3) (getDoubleN list1 4) (getDoubleN list1 5) (getDoubleN list1 6) (getDoubleN list1 7) (getDoubleN list1 8)
--data DatumDesc = Left Datum | Right DatumExt

getDatum (int1 , list1)
    | int1 < 999 = DatumDesc (Datum.getDatumById int1)
    | int1 == 999 = DatumDesc (getDatumFromList (int1 , list1))
    | int1 < 9999 =  DatumExtDesc (DatumExt.getDatumExtById int1)
    | int1 == 9999 =   DatumExtDesc (getDatumExtFromList (int1 , list1))
    | otherwise = NothingDesc

getParamTailByDatum (int1 , list1)
    | int1 < 999 = list1
    | int1 == 999 = getParamTail list1 3
    | int1 < 9999 = list1
    | int1 == 9999 = getParamTail list1 8
    | otherwise =  list1

getParamUnit list1 = (getUnitById  (getInteger (head list1)), tail list1)

getParamDoble list1 = (ParamDoble (getDouble (head list1)), tail list1)

--getCRSDesc text1 = 