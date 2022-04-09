module Param (Parameter(Parameter), parameterList, textParameter, showParameter, testParameter, getParametersById, getParameterById) where 

import qualified Data.Text as T

data Parameter = Parameter {
      id :: Integer
    , epsg :: Integer
    , proj :: T.Text
    , name :: T.Text
    , ruName :: T.Text
    , wktName :: T.Text
} deriving (Show)

textParameter :: Parameter -> T.Text
textParameter (Parameter i e n r w p) = str
                                        where str = mconcat [(T.pack . show) i, " ", (T.pack . show) e, " ", n, " ", r, " ", w, " ", p]
  
showParameter :: Parameter -> String
showParameter = show . T.unpack . textParameter

parameterList :: [Parameter]
parameterList = [ 
  Parameter 1 0 "Datum" "Датум" "Datum" "+datum", 
  Parameter 2 0 "Unit" "Ед. измерения" "Units" "+units", 
  Parameter 3 8802 "Longitude of natural origin" "Нулевая долгота" "Central_Meridian" "+lon_0",
  Parameter 4 8801 "Latitude of natural origin" "Нулевая широта" "Latitude_Of_Origin" "+lat_0",
  Parameter 5 8823 "Latitude of 1st standard parallel" "Стандартная параллель 1" "Standard_Parallel_1" "+lat_1",
  Parameter 6 8824 "Latitude of 2nd standard parallel" "Стандартная параллель 2" "Standard_Parallel_2" "+lat_2",
  Parameter 7 8813 "Azimuth of initial line" "Азимут" "Azimuth" "+alpha",
  Parameter 8 8805 "Scale factor at natural origin" "Масштабный множитель" "Scale_Factor" "+k_0",
  Parameter 9 8806 "False easting" "Восточное смещение" "False_Easting" "+x_0",
  Parameter 10 8807 "False northing" "Северное смещение" "False_Northing" "+y_0",
  Parameter 11 0 "Range" "Охват" "" ""
  ]

testParameter :: Parameter -> Integer -> Bool
testParameter (Parameter i _ _ _ _ _) k = i == k

getParametersById :: Integer -> [Parameter]
getParametersById x = filter (`testParameter` x) parameterList

getParameterById :: Integer -> Parameter
getParameterById = head . getParametersById