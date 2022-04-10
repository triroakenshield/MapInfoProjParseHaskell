module Datum where 

import qualified Data.Text as T

data Datum = Datum{
    id :: Integer,
    epsg :: Integer,
    proj :: T.Text,
    name :: T.Text,
    ellipsoidId :: Integer,
    x :: Double,
    y :: Double,
    z :: Double
}