module Ellipsoid where 

import qualified Data.Text as T
import Description ( getItemById )

data Ellipsoid = Ellipsoid {
    id :: Integer,
    epsg :: Integer,
    proj :: T.Text,
    name :: T.Text,
    a :: Double,
    f1 :: Double
}

textEllipsoid :: Ellipsoid -> T.Text
textEllipsoid (Ellipsoid i e pr na a f1) = str
                                        where str = mconcat [(T.pack . show) i, " ", (T.pack . show) e, " ", pr, " ", na, " ", (T.pack . show) a, " ", (T.pack . show) f1]
  
showEllipsoid :: Ellipsoid -> String
showEllipsoid = show . T.unpack . textEllipsoid

toProj :: Ellipsoid -> String
toProj (Ellipsoid i e pr na a f1) = if pr == ""   
                                     then "+a=" ++ show a ++ "+f=" ++ show f1
                                     else "+ellps=" ++ T.unpack pr

ellipsoidList :: [Ellipsoid]
ellipsoidList = [ 
    Ellipsoid 0  7019  "GRS 80"  "GRS80"  6378137  298.257222101, 
    Ellipsoid 1  7043  "WGS 72"  "WGS72"  6378135  298.26, 
    Ellipsoid 2  7003  "Australian"  "aust_SA"  6378160  298.25, 
    Ellipsoid 3  7024  "Krassovsky"  "krass"  6378245  298.3, 
    Ellipsoid 4  7022  "International 1924"  "intl"  6378388  297, 
    Ellipsoid 5  7022  "Hayford"  ""  6378388  297, 
    Ellipsoid 6  7012  "Clarke 1880"  ""  6378249.145  293.465, 
    Ellipsoid 7  7008  "Clarke 1866"  "clrk66"  6378206.4  294.9786982, 
    Ellipsoid 8  7009  "Clarke 1866 (modified for Michigan, "  ""  6378450.04748448  294.9786982, 
    Ellipsoid 9  7001  "Airy 1930"  "airy"  6377563.396  299.3249646, 
    Ellipsoid 10  7004  "Bessel 1841"  "bessel"  6377397.155  299.1528128, 
    Ellipsoid 11  7015  "Everest 1830"  "evrst30"  6377276.345  300.8017, 
    Ellipsoid 12  7035  "Sphere"  "sphere"  6370997  0, 
    Ellipsoid 13  7002  "Airy 1930 (modified for Ireland 1965, "  "mod_airy"  6377340.189  299.3249646, 
    Ellipsoid 14  7006  "Bessel 1841 (modified for Schwarzeck, "  "bess_nam"  6377483.865  299.1528128, 
    Ellipsoid 15  7013  "Clarke 1880 (modified for Arc 1950, "  "clrk80"  6378249.145326  293.4663076, 
    Ellipsoid 16  7014  "Clarke 1880 (modified for Merchich, "  ""  6378249.2  293.46598, 
    Ellipsoid 17  7018  "Everest 1830 (modified for Kertau, "  "evrst48"  6377304.063  300.8017, 
    Ellipsoid 18  (-1)  "Fischer 1960"  "fschr60"  6378166  298.3, 
    Ellipsoid 19  (-1)  "Fischer 1960 (modified for South Asia, "  "fschr60m"  6378155  298.3, 
    Ellipsoid 20  (-1)  "Fischer 1968"  "fschr68"  6378150  298.3, 
    Ellipsoid 21  7036  "GRS 67"  "GRS67"  6378160  298.247167427, 
    Ellipsoid 22  7020  "Helmert 1906"  "helmert"  6378200  298.3, 
    Ellipsoid 23  7053  "Hough"  "hough"  6378270  297, 
    Ellipsoid 24  7050  "South American"  ""  6378160  298.25, 
    Ellipsoid 25  7029  "War Office"  ""  6378300.583  296, 
    Ellipsoid 26  (-1)  "WGS 60"  "WGS60"  6378165  298.3, 
    Ellipsoid 27  7025  "WGS 66"  "WGS66"  6378145  298.25, 
    Ellipsoid 28  7030  "WGS 84"  "WGS84"  6378137  298.257223563, 
    Ellipsoid 30  7011  "Clarke 1880 (modified for IGN, "  "clrk80ign"  6378249.2  293.4660213, 
    Ellipsoid 31  7049  "IAG 75"  "IAU76"  6378140  298.257222, 
    Ellipsoid 32  (-1)  "MERIT 83"  "MERIT"  6378137  298.257, 
    Ellipsoid 33  (-1)  "New International 1967"  "new_intl"  6378157.5  298.25, 
    Ellipsoid 34  (-1)  "Walbeck"  "walbeck"  6376896  302.78, 
    Ellipsoid 35  7005  "Bessel 1841 (modified for NGO 1948, "  ""  6377492.0176  299.15281, 
    Ellipsoid 36  7007  "Clarke 1858"  ""  6378293.639  294.26068, 
    Ellipsoid 37  7013  "Clarke 1880 (modified for Jamaica, "  ""  6378249.136  293.46631, 
    Ellipsoid 38  7010  "Clarke 1880 (modified for Palestine, "  ""  6378300.79  293.46623, 
    Ellipsoid 39  7016  "Everest 1830 (modified for Timbalai, "  "evrstSS"  6377298.556  300.8017, 
    Ellipsoid 40  7044  "Everest 1830 (modified for Kalianpur, "  "evrst56"  6377301.243  300.80174, 
    Ellipsoid 41  7021  "Indonesian"  ""  6378160  298.247, 
    Ellipsoid 42  7025  "NWL 9D"  "NWL9D"  6378145  298.25, 
    Ellipsoid 43  7043  "NWL 10D"  ""  6378135  298.26, 
    Ellipsoid 44  7032  "OSU86F"  ""  6378136.2  298.25722, 
    Ellipsoid 45  7033  "OSU91A"  ""  6378136.3  298.25722, 
    Ellipsoid 46  7027  "Plessis 1817"  "plessis"  6376523  308.64, 
    Ellipsoid 47  7028  "Struve 1860"  ""  6378297  294.73, 
    Ellipsoid 48  7056  "Everest 1830 (modified for West Malaysia, "  "evrst69"  6377295.664  300.8017, 
    Ellipsoid 49  (-1)  "Irish (WOFO, "  ""  6377542.178  299.325, 
    Ellipsoid 50  (-1)  "Everest (Pakistan, "  ""  6377309.613  300.8017, 
    Ellipsoid 51  7041  "ATS 77 (Average Terrestrial System 1977, "  ""  6378135  298.257, 
    Ellipsoid 52  7054  "PZ90 (Russia, "  ""  6378136  298.257839303, 
    Ellipsoid 53  (-1)  "Xian 1980"  ""  6378140  298.25
  ]

testEllipsoid :: Ellipsoid -> Integer -> Bool
testEllipsoid (Ellipsoid i _ _ _ _ _) k = i == k

getEllipsoidById :: Integer -> Ellipsoid
getEllipsoidById = getItemById testEllipsoid ellipsoidList