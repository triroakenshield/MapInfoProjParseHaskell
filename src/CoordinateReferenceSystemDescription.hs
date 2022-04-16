module CoordinateReferenceSystemDescription where

import Data.Either
import qualified Data.Text as T

import Description
import Param
import Unit 
import Ellipsoid
import Datum
import DatumExt
import Projection 

data ParameterValue = DatumDesc Datum | DatumExtDesc DatumExt | UnitDesc Unit | ParamDesc ParamDoble | NothingDesc

data CoordinateReferenceSystemDescription = CoordinateReferenceSystemDescription {
    name :: T.Text,
    projection :: Projection,
    values :: [ParameterValue]
}

toProj (CoordinateReferenceSystemDescription _ pr val) = Projection.toProj pr ++ " " ++ unwords (map toProj1 val)

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

getDatum :: (Integer, [T.Text]) -> ParameterValue
getDatum (int1 , list1)
    | int1 < 999 = DatumDesc (Datum.getDatumById int1)
    | int1 == 999 = DatumDesc (getDatumFromList (int1 , list1))
    | int1 < 9999 =  DatumExtDesc (DatumExt.getDatumExtById int1)
    | int1 == 9999 =   DatumExtDesc (getDatumExtFromList (int1 , list1))
    | otherwise = NothingDesc

getParamTailByDatum :: (Integer, [T.Text]) -> [T.Text]
getParamTailByDatum (int1 , list1)
    | int1 < 999 = list1
    | int1 == 999 = getParamTail list1 3
    | int1 < 9999 = list1
    | int1 == 9999 = getParamTail list1 8
    | otherwise = list1

getIntegerVal list1  = (getInteger (head list1), tail list1)

getParamUnit list1 = (UnitDesc (getUnitById  (getInteger (head list1))), tail list1)

getParamDoble i list1 =  (ParamDesc (ParamDoble (getParameterById i)  (getDouble (head list1))), tail list1)

parseParam1 :: (Integer, [T.Text]) -> (ParameterValue, [T.Text])
parseParam1 (i, list1)
    | i == 1 = (getDatum para1,  getParamTailByDatum  para1)
    | i == 2 = getParamUnit list1
    | otherwise = getParamDoble i list1
    where  para1 = getIntegerVal list1

parseNaxtParam :: ([Integer], [T.Text], [ParameterValue]) -> ([Integer], [T.Text], [ParameterValue])
parseNaxtParam (iList, tList, plist) = addParam (iList, tList, plist) (parseParam1 (head iList , tList))

addParam :: ([Integer], [T.Text], [ParameterValue]) -> (ParameterValue, [T.Text]) -> ([Integer], [T.Text], [ParameterValue])
addParam (iList, tList, plist) (pv, nList) = (tail iList , nList , plist ++ [pv])

parseAllParam (iList, tList, plist) = if null tList
                                      then plist
                                      else parseAllParam (parseNaxtParam (iList, tList, plist))

toProj1 :: ParameterValue -> String
toProj1 (DatumDesc d1) =  Datum.toProj d1
toProj1 (DatumExtDesc d2) =  DatumExt.toProj d2
toProj1 (UnitDesc u1) =  Unit.toProj u1
toProj1 (ParamDesc (ParamDoble (Parameter _ _ _ _ _ p) v)) = T.unpack p ++ "=" ++ show v
toProj1 NothingDesc =  ""

getProjectionParams :: Projection -> [T.Text] -> [ParameterValue]
getProjectionParams (Projection _ _ _ _ _ _ iList) list1 = parseAllParam (iList, list1, [])

getCRS1 name proj list = CoordinateReferenceSystemDescription name proj (getProjectionParams proj list)

getCRS2 list = getCRS1 (getName list) (getProjection (head params1)) (tail params1)
               where params1 = getParameters list