module Projection where

import qualified Data.Text as T
import Description

data Unit = Unit {
    id :: Integer,
    epsg :: Integer,
    proj :: T.Text,
    name :: T.Text,
    ruName :: T.Text,
    b :: Double,
    c :: Double,
    targetEpsg :: Integer
}

textUnit :: Unit -> T.Text
textUnit (Unit i e pr na rn b c te) = str
                                      where str = mconcat [(T.pack . show) i, " ", (T.pack . show) e, " ", pr, " ", na, " ", rn, " ", (T.pack . show) b, " ", (T.pack . show) c, " ", te]
  
showUnit :: Unit -> String
showUnit = show . T.unpack . textUnit

toProj :: Unit -> String
toProj (Unit _ _ pr _ _ _ _ _) = "+units=" ++ T.unpack pr

unitList :: [Unit]
unitList = [
    Unit 0 9035 "mi" "Miles" "мили" 63360 39.37 9001,
    Unit 1 9036 "km" "Kilometers" "Километры" 1000 1 9001,
    Unit 2 (-1) "in" "Inches" "Дюймы" 1 39.37 9001,
    Unit 3 9002 "ft" "Feet (also called International Feet," "Фут (также называется международный фут, Один международный фут равен точно 30.48 см" 0.3048 1 9001,
    Unit 4 9096 "yd" "Yards" "ярд" 0.9144 1 9001,
    Unit 5 1025 "mm" "Millimeters" "миллиметры" 1 1000 9001,
    Unit 6 1033 "cm" "Centimeters" "Сантиметры" 1 100 9001,
    Unit 7 9001 "m" "Meters" "метры" 1 1 9001,
    Unit 8 9003 "us-ft" "US Survey Feet (used for 1927 State Plane," "геодезический фут США (принятый для плановых систем штатов 1927, один геодезический фут США равен точно 12/39.37 метра или прблизительно 30.48006 см" 12 39.37 9001,
    Unit 9 9030 "kmi" "Nautical Miles" "морская миля (одна морская миля равно точно 1852 метрам," 1852 1 9001,
    Unit 30 9098 "link" "Links" "линк" 20.1168 100 9001,
    Unit 31 9097 "ch" "Chains" "чейн" 20.1168 1 9001,
    Unit 32 (-1) "rod" "Rods" "род" 33 2 9003
    ]