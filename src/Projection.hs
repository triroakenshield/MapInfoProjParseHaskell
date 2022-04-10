module Projection where

import qualified Data.Text as T
import Description ( getItemById )

data Projection = Projection {
    id :: Integer
    , epsg :: Integer
    , name :: T.Text
    , ruName :: T.Text
    , wktName :: T.Text
    , proj :: T.Text
    , parameters :: [Integer]
} deriving (Show)

textProjection :: Projection -> T.Text
textProjection (Projection i e n r w p ps) = str
                                             where str = mconcat [(T.pack . show) i, " ", (T.pack . show) e, " ", n, " ", r, " ", w, " ", p]
  
showProjection :: Projection -> String
showProjection = show . T.unpack . textProjection

projectionList :: [Projection]
projectionList = [
    Projection 1 4326 "Longitude/Latitude" "Долгота-Широта" "" "lonlat" [1],
    Projection 2 4326 "Cylindrical Equal-Area" "Равноплощадная цилиндрическая" "" "cea" [1, 2, 3, 5],
    Projection 3 9801 "Lambert Conformal Conic" "Равноугольная коническая проекция Ламберта" "Lambert conformal conic" "lcc" [1, 2, 3, 4, 5, 6, 9, 10],
    Projection 4 9820 "Lambert Azimuthal Equal-Area (polar aspect only)" "Равноплощадная азимутальная Ламберта (только в полярной области)" "" "laea" [1, 2, 3, 4, 11],
    Projection 5 4326 "Azimuthal Equidistant (polar aspect only)" "Равнопромежуточная коническая (только для полярных областей)" "Azimuthal or Planar Projections " "aeqd" [1, 2, 3, 4, 11],
    Projection 6 54027 "Equidistant Conic also known as Simple Conic" "Равнопромежуточная коническая" "" "eqdc" [1, 2, 3, 4, 5, 6, 9, 10],
    Projection 7 9812 "Hotine Oblique Mercator" "Косая Меркатора – Хотина" "Oblique Mercator" "omerc" [1, 2, 3, 4, 7, 8, 9, 10],
    Projection 8 9807 "Transverse Mercator (also known as Gauss-Kruger)" "Поперечная Меркатора" "Gauss-Kruger Transverse Mercator" "tmerc" [1, 2, 3, 4, 8, 9, 10],
    Projection 9 9822 "Albers Equal-Area Conic" "Коническая равноплощадная Алберса" "Albers conic equal-area" "aea" [1, 2, 3, 4, 5, 6, 9, 10],
    Projection 10 9804 "Mercator" "Меркатора" "Mercator" "merc" [1, 2, 3],
    Projection 11 54003 "Miller Cylindrical" "Миллера" "" "mill" [1, 2, 3],
    Projection 12 54030 "Robinson" "Робинсона" "" "robin" [1, 2, 3],
    Projection 13 54009 "Mollweide" "Мольвейде" "" "moll" [1, 2, 3],
    Projection 14 54012 "Eckert IV" "Эккерта IV" "" "eck4" [1, 2, 3],
    Projection 15 54010 "Eckert VI" "Эккерта VI" "" "eck6" [1, 2, 3],
    Projection 16 54008 "Sinusoidal" "Синусоидальная" "" "sinu " [1, 2, 3],
    Projection 17 54016 "Gall" "Галла" "" "gall" [1, 2, 3],
    Projection 18 27200 " Zealand Map Grid" "Новозеландская картографическая" "" "nzmg" [1, 2, 3, 4, 9, 10],
    Projection 19 0 "Lambert Conformal Conic (modified for Belgium 1972)" "Равноугольная коническая Ламберта (для Бельгии 1972)" "" "lcca" [1, 2, 3, 4, 5, 6, 9, 10],
    Projection 20 54026 "Stereographic" "Стереографическая" "Stereographic" "stere" [1, 2, 3, 4, 8, 9, 10],
    Projection 21 0 "Transverse Mercator (modified for Danish System 34 Jylland-Fyn)" "Поперечная Меркатора (для голландской системы 34 для района Юланд-Фин)" "" "tmerc" [1, 2, 3, 4, 8, 9, 10],
    Projection 22 0 "Transverse Mercator (modified for Danish System 34 Sjaelland)" "Поперечная Меркатора (зона 34 Голландии Съеланд)" "" "tmerc" [1, 2, 3, 4, 8, 9, 10],
    Projection 23 0 "Transverse Mercator (modified for Danish System 34/45 Bornholm)" "Поперечная Меркатора (34/35 зоны для Голландии: Борнхольм)" "" "tmerc" [1, 2, 3, 4, 8, 9, 10],
    Projection 24 0 "Transverse Mercator (modified for Finnish KKJ)" "Поперечная проекция Меркатора (для Финляндии KKJ)" "" "tmerc" [1, 2, 3, 4, 8, 9, 10],
    Projection 25 9815 "Swiss Oblique Mercator" "Косая Меркатора для Швейцарии" "" "somerc" [1, 2, 3, 4, 9, 10],
    Projection 26 0 "Regional Mercator" "Региональная Меркатора" "" "tmerc" [1, 2, 3, 5],
    Projection 27 9818 "Polyconic" "Поликоническая" "American Polyconic ?" "poly " [1, 2, 3, 4, 9, 10],
    Projection 28 4326 "Azimuthal Equidistant (all origin latitudes)" "Равнопромежуточная коническая" "" "aeqd" [1, 2, 3, 4, 11],
    Projection 29 9820 "Lambert Azimuthal Equal-Area" "Равноплощадная азимутальная Ламберта" "Lambert Azimuthal Equal Area" "laea" [1, 2, 3, 4, 11],
    Projection 30 9806 "Cassini-Soldner" "Кассини-Солднера" "Cassini-Soldner" "cass" [1, 2, 3, 4, 9, 10],
    Projection 31 9809 "Double Stereographic" "Двойная стереографическая" "Oblique stereographic" "sterea" [1, 2, 3, 4, 8, 9, 10],
    Projection 32 9819 "Krovak Oblique Conformal Conic (JTSKc)" "Косая равноугольная коническая проекция Кровак (JTSKc)" "" "krovak" [1, 2, 3, 4, 5, 7, 9, 10],
    Projection 33 9842 "Equidistant Cylindrical" "Равнопромежуточная цилиндрическая" "" "eqc" [1, 2, 3, 5, 9, 10]
    ]

testProjection :: Projection -> Integer -> Bool
testProjection (Projection i _ _ _ _ _ _) k = i == k

getProjectionById :: Integer -> Projection
getProjectionById = getItemById testProjection projectionList