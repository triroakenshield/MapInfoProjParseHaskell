module DatumExt where 

import qualified Data.Text as T
import Ellipsoid ( Ellipsoid, getEllipsoidById, toProj )

data DatumExt = DatumExt {
    id :: Integer,
    epsg :: Integer,
    proj :: T.Text,
    name :: T.Text,
    ellipsoid :: Ellipsoid,
    x :: Double,
    y :: Double,
    z :: Double,
    rotationX :: Double,
    rotationY :: Double,
    rotationZ :: Double,
    scalePpm :: Double,
    primeMeridian :: Double
}

textDatumExt :: DatumExt -> T.Text
textDatumExt (DatumExt i e pr na el x y z rx ry rz s pm) = str
                                      where str = mconcat [(T.pack . show) i, " ", (T.pack . show) e, " ", pr, " ", na]
  
showDatumExt :: DatumExt -> String
showDatumExt = show . T.unpack . textDatumExt

getXYZ :: (Show a1, Show a2, Show a3) => a1 -> a2 -> a3 -> [Char]
getXYZ x y z = "" ++ show x ++ "," ++ show y ++ "," ++ show z


getRotation rx ry rz s = "" ++ show (rx * (-1)) ++ "," ++ show (rx * (-1)) ++ "," ++ show (rz * (-1)) ++ "," ++ show s

getPrimeMeridian pm = if pm == 0
                       then ""
                       else "+pm=" ++ show pm

toProj :: DatumExt -> String
toProj (DatumExt _ _ pr _ el x y z rx ry rz s pm) = if pr == ""
                                                     then Ellipsoid.toProj el ++ "+towgs84=" ++ getXYZ x y z ++ "," ++ getRotation rx ry rz s ++ getPrimeMeridian pm
                                                     else "+datum=" ++ T.unpack pr

datumExtList :: [DatumExt]
datumExtList = [
    DatumExt 1001 6284 "Pulkovo 1942" "" (getEllipsoidById 3) 24 (-123) (-94) (-0.02) 0.25 0.13 1.1 0,
    DatumExt 1002 6807 "NTF (Paris meridian," "" (getEllipsoidById 30) (-168) (-60) 320 0 0 0 0 2.33723,
    DatumExt 1003 6149 "CH 1903 (Switzerland," "" (getEllipsoidById 10) 660.077 13.551 369.344 0.804816 0.577692 0.952236 5.66 0,
    DatumExt 1004 6237 "HD72 (Hungarian Datum of 1972," "" (getEllipsoidById 21) (-56) 75.77 15.31 (-0.37) (-0.2) (-0.21) (-1.01) 0,
    DatumExt 1005 (-1) "Cape (South Africa," "" (getEllipsoidById 28) (-134.73) (-110.92) (-292.66) 0 0 0 1 0,
    DatumExt 1006 (-1) "Australia National (AGD84," "" (getEllipsoidById 2) (-117.763) (-51.51) 139.061 (-0.292) (-0.443) (-0.277) (-0.191) 0,
    DatumExt 1007 (-1) "Australia A.C.T. (AGD66," "" (getEllipsoidById 2) (-129.193) (-41.212) 130.73 (-0.246) (-0.374) (-0.329) (-2.955) 0,
    DatumExt 1008 (-1) "Australia Tasmania (AGD66," "" (getEllipsoidById 2) (-120.271) (-64.543) 161.632 (-0.2175) 0.0672 0.1291 2.4985 0,
    DatumExt 1009 (-1) "Australia Victoria/NSW (AGD66," "" (getEllipsoidById 2) (-119.353) (-48.301) 139.484 (-0.415) (-0.26) (-0.437) (-0.613) 0,
    DatumExt 1010 6272 "New Zealand Geodetic Datum 1949" "nzgd49" (getEllipsoidById 4) 59.47 (-5.04) 187.44 (-0.47) 0.1 (-1.024) (-4.5993) 0,
    DatumExt 1011 (-1) "Sweden (RT 90," "" (getEllipsoidById 10) 419.384 99.3335 591.345 (-0.850389) (-1.81728) 7.86224 (-0.99496) 0,
    DatumExt 1012 (-1) "Russia PZ90" "" (getEllipsoidById 52) (-1.08) (-0.27) (-0.9) 0 0 (-0.16) (-0.12) 0,
    DatumExt 1013 (-1) "Russia SK42" "" (getEllipsoidById 52) 23.92 (-141.27) (-80.9) 0 (-0.35) (-0.82) (-0.12) 0,
    DatumExt 1014 (-1) "Russia SK95" "" (getEllipsoidById 52) 24.82 (-131.21) (-82.66) 0 0 (-0.16) (-0.12) 0,
    DatumExt 1015 (-1) "Tokyo97" "" (getEllipsoidById 10) (-146.414) 507.337 680.507 0 0 0 0 0,
    DatumExt 1016 (-1) "KKJ" "" (getEllipsoidById 4) (-96.062) (-82.428) (-121.754) (-4.801) (-0.345) 1.376 1.496 0,
    DatumExt 1017 6610 "Xian 1980" "" (getEllipsoidById 53) 24 (-123) (-94) (-0.02) (-0.25) 0.13 1.1 0,
    DatumExt 1018 (-1) "Lithuanian Pulkovo 1942" "" (getEllipsoidById 3) (-40.5953) (-18.5498) (-69.3396) (-2.508) (-1.8319) 2.6114 (-4.2991) 0,
    DatumExt 1019 (-1) "Belgian 1972 7 Parameter" "" (getEllipsoidById 4) (-99.059) 53.322 (-112.486) (-0.419) 0.83 (-1.885) 0.999999 0
    ]