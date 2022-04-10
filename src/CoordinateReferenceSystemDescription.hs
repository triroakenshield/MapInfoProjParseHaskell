module CoordinateReferenceSystemDescription where

import qualified Data.Text as T

import Projection ( Projection(Projection), getProjectionById )

data CoordinateReferenceSystemDescription = CoordinateReferenceSystemDescription {
    name :: T.Text,
    projection :: Projection
}

sWord :: T.Text
sWord = T.pack "\""

list12 :: T.Text -> (T.Text, T.Text)
list12 = T.breakOnEnd sWord

getName :: T.Text -> T.Text
getName = fst . list12

getTail :: T.Text -> T.Text
getTail = snd . list12

sep :: T.Text
sep = T.pack ","

getParameters :: T.Text -> [T.Text]
getParameters = T.splitOn sep . getTail

getParameterStr :: Int -> T.Text -> T.Text
getParameterStr id text = getParameters text !! id

getInteger :: String -> Integer
getInteger x = read x :: Integer

getParameterId :: T.Text -> Integer
getParameterId = getInteger . T.unpack . getParameterStr 1

testProjectionId :: Integer -> Integer
testProjectionId res  
             | res > 2999 = res - 3000 
             | res > 1999 = res - 2000 
             | res > 999  = res - 1000
             | otherwise = res

getProjection :: T.Text -> Projection
getProjection = getProjectionById . testProjectionId . getParameterId