module Main where

import qualified Data.Text as T

import Description
import CoordinateReferenceSystemDescription

aWord :: T.Text
aWord = T.pack "\"МСК-66 зона 1, 3 градусная\", 8, 1001, 7, 60.05, 0, 1, 1500000, -5911057.63"

aWord1 :: T.Text
aWord1 =  T.pack "\"МСК-27 зона 1\", 8, 9999, 3, 23.57, -140.95, -79.8, 0, -0.35, -0.79, -0.22, 0, 7, 130.71666666666, 0, 1, 1300000, -4916586.44"

crs :: CoordinateReferenceSystemDescription
crs = getCRS2 aWord1

itogStr :: String
itogStr = CoordinateReferenceSystemDescription.toProj crs

main :: IO ()
main = putStrLn itogStr