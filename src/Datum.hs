module Datum where 

import qualified Data.Text as T
import Description
import Ellipsoid

data Datum = Datum{
    id :: Integer,
    epsg :: Integer,
    proj :: T.Text,
    name :: T.Text,
    ellipsoid :: Ellipsoid,
    x :: Double,
    y :: Double,
    z :: Double
}

textDatum :: Datum -> T.Text
textDatum (Datum i e pr na el x y z) = str
                                      where str = mconcat [(T.pack . show) i, " ", (T.pack . show) e, " ", pr, " ", na]
  
showDatum :: Datum -> String
showDatum = show . T.unpack . textDatum

toProj :: Datum -> String
toProj (Datum _ _ pr _ el x y z) = if pr == ""
                                   then Ellipsoid.toProj el ++ " +towgs84=" ++ show x ++ "," ++ show y ++ "," ++ show z
                                   else " +datum=" ++ T.unpack pr

datumList :: [Datum]
datumList = [
    Datum 1 6201 "Adindan" "" (getEllipsoidById 6) (-162) (-12) 206,
    Datum 2 6205 "Afgooye" "" (getEllipsoidById 3) (-43) (-163) 45,
    Datum 3 6204 "AinelAbd1970" "" (getEllipsoidById 4) (-150) (-251) (-2),
    Datum 4 6708 "Anna1Astro1965" "" (getEllipsoidById 2) (-491) (-22) 435,
    Datum 5 6209 "Arc1950" "" (getEllipsoidById 15) (-143) (-90) (-294),
    Datum 6 6210 "Arc1960" "" (getEllipsoidById 6) (-160) (-8) (-300),
    Datum 7 6712 "AscensionIsland1958" "" (getEllipsoidById 4) (-207) 107 52,
    Datum 8 (-1) "AstroBeacon \"E\"" "" (getEllipsoidById 4) 145 75 (-272),
    Datum 9 (-1) "AstroB4SorolAtoll" "" (getEllipsoidById 4) 114 (-116) (-333),
    Datum 10 (-1) "AstroDOS71/4" "" (getEllipsoidById 4) (-320) 550 (-494),
    Datum 11 (-1) "AstronomicStation1952" "" (getEllipsoidById 4) 124 (-234) (-25),
    Datum 12 6202 "AustralianGeodetic1966" "" (getEllipsoidById 2) (-133) (-48) 148,
    Datum 13 6203 "AustralianGeodetic1984" "" (getEllipsoidById 2) (-134) (-48) 149,
    Datum 14 6714 "Bellevue(IGN," "" (getEllipsoidById 4) (-127) (-769) 472,
    Datum 15 6216 "Bermuda1957" "" (getEllipsoidById 7) (-73) 213 296,
    Datum 16 (-1) "BogotaObservatory" "" (getEllipsoidById 4) 307 304 (-318),
    Datum 17 6221 "CampoInchauspe" "" (getEllipsoidById 4) (-148) 136 90,
    Datum 18 (-1) "CantonAstro1966" "" (getEllipsoidById 4) 298 (-304) (-375),
    Datum 19 6222 "Cape" "" (getEllipsoidById 6) (-136) (-108) (-292),
    Datum 20 6717 "CapeCanaveral" "" (getEllipsoidById 7) (-2) 150 181,
    Datum 21 6223 "Carthage" "carthage" (getEllipsoidById 6) (-263) 6 431,
    Datum 22 6672 "Chatham1971" "" (getEllipsoidById 4) 175 (-38) 113,
    Datum 23 6224 "ChuaAstro" "" (getEllipsoidById 4) (-134) 229 (-29),
    Datum 24 1074 "CorregoAlegre" "" (getEllipsoidById 4) (-206) 172 (-6),
    Datum 25 6813 "Djakarta(Batavia," "" (getEllipsoidById 10) (-377) 681 (-50),
    Datum 26 (-1) "DOS1968" "" (getEllipsoidById 4) 230 (-199) (-752),
    Datum 27 6719 "EasterIsland1967" "" (getEllipsoidById 4) 211 147 111,
    Datum 28 6230 "European1950" "" (getEllipsoidById 4) (-87) (-98) (-121),
    Datum 29 6668 "European1979" "" (getEllipsoidById 4) (-86) (-98) (-119),
    Datum 30 6685 "GandajikaBase" "" (getEllipsoidById 4) (-133) (-321) 50,
    Datum 31 (-1) "GeodeticDatum1949" "" (getEllipsoidById 4) 84 (-22) 209,
    Datum 32 (-1) "GRS67" "" (getEllipsoidById 21) 0 0 0,
    Datum 33 (-1) "GRS80" "" (getEllipsoidById 0) 0 0 0,
    Datum 34 6675 "Guam1963" "" (getEllipsoidById 7) (-100) (-248) 259,
    Datum 35 (-1) "GUX1Astro" "" (getEllipsoidById 4) 252 (-209) (-751),
    Datum 36 6254 "HitoXVIII1963" "" (getEllipsoidById 4) 16 196 93,
    Datum 37 6658 "Hjorsey1955" "" (getEllipsoidById 4) (-73) 46 (-86),
    Datum 38 6738 "HongKong1963" "" (getEllipsoidById 4) (-156) (-271) (-189),
    Datum 39 6236 "Hu-Tzu-Shan" "" (getEllipsoidById 4) (-634) (-549) (-201),
    Datum 40 (-1) "Indian(Thailand/Vietnam," "" (getEllipsoidById 11) 214 836 303,
    Datum 41 (-1) "Indian(Bangladesh" "" (getEllipsoidById 11) 289 734 257,
    Datum 42 (-1) "Ireland1965" "" (getEllipsoidById 13) 506 (-122) 611,
    Datum 43 (-1) "ISTS073Astro1969" "" (getEllipsoidById 4) 208 (-435) (-229),
    Datum 44 6725 "JohnstonIsland1961" "" (getEllipsoidById 4) 191 (-77) (-204),
    Datum 45 6244 "Kandawala" "" (getEllipsoidById 11) (-97) 787 86,
    Datum 46 (-1) "KerguelenIsland" "" (getEllipsoidById 4) 145 (-187) 103,
    Datum 47 6751 "Kertau1948" "" (getEllipsoidById 17) (-11) 851 5,
    Datum 48 (-1) "L.C.5Astro" "" (getEllipsoidById 7) 42 124 147,
    Datum 49 6251 "Liberia1964" "" (getEllipsoidById 6) (-90) 40 88,
    Datum 50 (-1) "Luzon(Philippines," "" (getEllipsoidById 7) (-133) (-77) (-51),
    Datum 51 (-1) "Luzon(MindanaoIsland," "" (getEllipsoidById 7) (-133) (-79) (-72),
    Datum 52 6256 "Mahe1971" "" (getEllipsoidById 6) 41 (-220) (-134),
    Datum 53 (-1) "MarcoAstro" "" (getEllipsoidById 4) (-289) (-124) 60,
    Datum 54 6262 "Massawa" "" (getEllipsoidById 10) 639 405 60,
    Datum 55 6261 "Merchich" "" (getEllipsoidById 16) 31 146 47,
    Datum 56 6727 "MidwayAstro1961" "" (getEllipsoidById 4) 912 (-58) 1227,
    Datum 57 6263 "Minna" "" (getEllipsoidById 6) (-92) (-93) 122,
    Datum 58 (-1) "Nahrwan(MasirahIsland," "" (getEllipsoidById 6) (-247) (-148) 369,
    Datum 59 (-1) "Nahrwan(Un.ArabEmirates," "" (getEllipsoidById 6) (-249) (-156) 381,
    Datum 60 (-1) "Nahrwan(SaudiArabia," "" (getEllipsoidById 6) (-231) (-196) 482,
    Datum 61 (-1) "Naparima" "" (getEllipsoidById 4) (-2) 374 172,
    Datum 62 (-1) "NAD27(ContinentalUS," "NAD27" (getEllipsoidById 7) (-8) 160 176,
    Datum 63 (-1) "NAD27(Alaska," "NAD27" (getEllipsoidById 7) (-5) 135 172,
    Datum 64 (-1) "NAD27(Bahamas," "NAD27" (getEllipsoidById 7) (-4) 154 178,
    Datum 65 (-1) "NAD27(SanSalvador," "NAD27" (getEllipsoidById 7) 1 140 165,
    Datum 66 (-1) "NAD27(Canada," "NAD27" (getEllipsoidById 7) (-10) 158 187,
    Datum 67 (-1) "NAD27(CanalZone," "NAD27" (getEllipsoidById 7) 0 125 201,
    Datum 68 (-1) "NAD27(Caribbean," "NAD27" (getEllipsoidById 7) (-7) 152 178,
    Datum 69 (-1) "NAD27(CentralAmerica," "NAD27" (getEllipsoidById 7) 0 125 194,
    Datum 70 (-1) "NAD27(Cuba," "NAD27" (getEllipsoidById 7) (-9) 152 178,
    Datum 71 (-1) "NAD27(Greenland," "NAD27" (getEllipsoidById 7) 11 114 195,
    Datum 72 (-1) "NAD27(Mexico," "NAD27" (getEllipsoidById 7) (-12) 130 190,
    Datum 73 (-1) "NAD27(Michigan," "NAD27" (getEllipsoidById 8) (-8) 160 176,
    Datum 74 (-1) "NAD83" "NAD83" (getEllipsoidById 0) 0 0 0,
    Datum 75 6129 "Observatorio1966" "" (getEllipsoidById 4) (-425) (-169) 81,
    Datum 76 (-1) "OldEgyptian" "" (getEllipsoidById 22) (-130) 110 (-13),
    Datum 77 6135 "OldHawaiian" "" (getEllipsoidById 7) 61 (-285) (-181),
    Datum 78 (-1) "Oman" "" (getEllipsoidById 6) (-346) (-1) 224,
    Datum 79 5101 "OrdnanceSurveyGreatBrit." "" (getEllipsoidById 9) 375 (-111) 431,
    Datum 80 6728 "PicodelasNieves" "" (getEllipsoidById 4) (-307) (-92) 127,
    Datum 81 6729 "PitcairnAstro1967" "" (getEllipsoidById 4) 185 165 42,
    Datum 82 6248 "ProvisionalSouthAmerican" "" (getEllipsoidById 4) (-288) 175 (-376),
    Datum 83 6139 "PuertoRico" "" (getEllipsoidById 7) 11 72 (-101),
    Datum 84 6614 "QatarNational" "" (getEllipsoidById 4) (-128) (-283) 22,
    Datum 85 6287 "Qornoq" "" (getEllipsoidById 4) 164 138 (-189),
    Datum 86 6626 "Reunion" "" (getEllipsoidById 4) 94 (-948) (-1262),
    Datum 87 (-1) "Rome1940" "" (getEllipsoidById 4) (-225) (-65) 9,
    Datum 88 (-1) "Santo(DOS," "" (getEllipsoidById 4) 170 42 84,
    Datum 89 (-1) "SaoBraz" "" (getEllipsoidById 4) (-203) 141 53,
    Datum 90 6292 "SapperHill1943" "" (getEllipsoidById 4) (-355) 16 74,
    Datum 91 6293 "Schwarzeck" "" (getEllipsoidById 14) 616 97 (-251),
    Datum 92 6291 "SouthAmerican1969" "" (getEllipsoidById 24) (-57) 1 (-41),
    Datum 93 (-1) "SouthAsia" "" (getEllipsoidById 19) 7 (-10) (-26),
    Datum 94 (-1) "SoutheastBase" "" (getEllipsoidById 4) (-499) (-249) 314,
    Datum 95 (-1) "SouthwestBase" "" (getEllipsoidById 4) (-104) 167 (-38),
    Datum 96 6298 "Timbalai1948" "" (getEllipsoidById 11) (-689) 691 (-46),
    Datum 97 6301 "Tokyo" "" (getEllipsoidById 10) (-128) 481 664,
    Datum 98 6734 "TristanAstro1968" "" (getEllipsoidById 4) (-632) 438 (-609),
    Datum 99 6731 "VitiLevu1916" "" (getEllipsoidById 6) 51 391 (-36),
    Datum 100 (-1) "Wake-Eniwetok1960" "" (getEllipsoidById 23) 101 52 (-39),
    Datum 101 (-1) "WGS60" "" (getEllipsoidById 26) 0 0 0,
    Datum 102 6760 "WGS66" "" (getEllipsoidById 27) 0 0 0,
    Datum 103 6322 "WGS72" "" (getEllipsoidById 1) 0 8 10,
    Datum 104 6326 "WGS84" "WGS84" (getEllipsoidById 28) 0 0 0,
    Datum 105 6309 "Yacare" "" (getEllipsoidById 4) (-155) 171 37,
    Datum 106 6311 "Zanderij" "" (getEllipsoidById 4) (-265) 120 (-358),
    Datum 107 6275 "NTF(Greenwichmeridian," "" (getEllipsoidById 30) (-168) (-60) 320,
    Datum 108 6231 "European1987" "" (getEllipsoidById 4) (-83) (-96) (-113),
    Datum 109 (-1) "NetherlandsBessel" "" (getEllipsoidById 10) 593 26 478,
    Datum 110 (-1) "BelgiumHayford" "" (getEllipsoidById 4) 81 120 129,
    Datum 111 (-1) "NWGL10" "" (getEllipsoidById 1) (-1) 15 1,
    Datum 112 (-1) "RT90(Sweden," "" (getEllipsoidById 10) 498 (-36) 568,
    Datum 113 (-1) "Lisboa(DLx," "" (getEllipsoidById 4) (-303) (-62) 105,
    Datum 114 (-1) "Melrica1973(D73," "" (getEllipsoidById 4) (-223) 110 37,
    Datum 115 6258 "EUREF89" "" (getEllipsoidById 0) 0 0 0,
    Datum 116 6283 "GDA94" "" (getEllipsoidById 0) 0 0 0,
    Datum 117 6167 "NZGD2000" "" (getEllipsoidById 0) 0 0 0,
    Datum 118 6169 "AmericanSamoa" "" (getEllipsoidById 7) (-115) 118 426,
    Datum 119 6601 "AntiguaIslandAstro1943" "" (getEllipsoidById 6) (-270) 13 62,
    Datum 120 6713 "AyabelleLighthouse" "" (getEllipsoidById 6) (-79) (-129) 145,
    Datum 121 6219 "BukitRimpah" "" (getEllipsoidById 10) (-384) 664 (-48),
    Datum 122 (-1) "Estonia1937" "" (getEllipsoidById 10) 374 150 588,
    Datum 123 6155 "Dabola" "" (getEllipsoidById 6) (-83) 37 124,
    Datum 124 6736 "DeceptionIsland" "" (getEllipsoidById 6) 260 12 (-147),
    Datum 125 (-1) "FortThomas1955" "" (getEllipsoidById 6) (-7) 215 225,
    Datum 126 (-1) "GraciosaBaseSW1948" "" (getEllipsoidById 4) (-104) 167 (-38),
    Datum 127 6255 "HeratNorth" "" (getEllipsoidById 4) (-333) (-222) 114,
    Datum 128 (-1) "Hermannskogel" "" (getEllipsoidById 10) 682 (-203) 480,
    Datum 129 (-1) "Indian(Pakistan," "" (getEllipsoidById 50) 283 682 231,
    Datum 130 6239 "Indian1954" "" (getEllipsoidById 11) 217 823 299,
    Datum 131 6131 "Indian1960" "" (getEllipsoidById 11) 198 881 317,
    Datum 132 6240 "Indian1975" "" (getEllipsoidById 11) 210 814 289,
    Datum 133 6238 "Indonesian1974" "" (getEllipsoidById 41) (-24) (-15) 5,
    Datum 134 (-1) "ISTS061Astro1968" "" (getEllipsoidById 4) (-794) 119 (-298),
    Datum 135 6735 "KusaieAstro1951" "" (getEllipsoidById 4) 647 1777 (-1124),
    Datum 136 6250 "Leigon" "" (getEllipsoidById 6) (-130) 29 364,
    Datum 137 6604 "MontserratIsl.Astro1958" "" (getEllipsoidById 6) 174 359 365,
    Datum 138 6266 "M'Poraloko" "" (getEllipsoidById 6) (-74) (-130) 42,
    Datum 139 6307 "NorthSahara1959" "" (getEllipsoidById 6) (-186) (-93) 310,
    Datum 140 (-1) "ObservatorioMeteor.1939" "" (getEllipsoidById 4) (-425) (-169) 81,
    Datum 141 6620 "Point58" "" (getEllipsoidById 6) (-106) (-129) 165,
    Datum 142 (-1) "PointeNoire1948" "" (getEllipsoidById 6) (-148) 51 (-291),
    Datum 143 6615 "PortoSanto1936" "" (getEllipsoidById 4) (-499) (-249) 314,
    Datum 144 6616 "SelvagemGrande1938" "" (getEllipsoidById 4) (-289) (-124) 60,
    Datum 145 (-1) "SierraLeone1960" "" (getEllipsoidById 6) (-88) 4 101,
    Datum 146 (-1) "S-JTSK" "" (getEllipsoidById 10) 589 76 480,
    Datum 147 6297 "TananariveObservatory1925" "" (getEllipsoidById 4) (-189) (-242) (-91),
    Datum 148 6304 "Voirol1874" "" (getEllipsoidById 6) (-73) (-247) 227,
    Datum 149 (-1) "Voirol1960" "" (getEllipsoidById 6) (-123) (-206) 219,
    Datum 150 6148 "Hartbeesthoek94" "" (getEllipsoidById 28) 0 0 0,
    Datum 151 6122 "ATS77" "" (getEllipsoidById 51) 0 0 0,
    Datum 152 6612 "JGD2000" "" (getEllipsoidById 0) 0 0 0,
    Datum 153 (-1) "HGRS87" "GGRS87" (getEllipsoidById 0) (-199.87) 74.79 246.62,
    Datum 154 6214 "Beijing 1954" "" (getEllipsoidById 3) (-31.4) 144.3 81.2
    ]

testDatum :: Datum -> Integer -> Bool
testDatum (Datum i _ _ _ _ _ _ _) k = i == k

getDatumById :: Integer -> Datum
getDatumById = getItemById testDatum datumList