import Description

import qualified Data.Text as T

assert :: Bool -> String -> String -> IO ()
assert test passStatement failStatement = if test 
    then putStrLn passStatement 
    else putStrLn failStatement

testStr1 :: T.Text
testStr1 = T.pack "1111"

test1 :: T.Text -> Bool
test1 x = getInteger x == (read (T.unpack x) :: Integer)

test2 d = 1 == 1

main :: IO ()
main = do 
       putStrLn "Запуск тестов..."

       --assert (test1 testStr1) "пройдено: ’доход’" "провал: ’доход’"

       assert (test1 testStr1) "good" "bad"

       putStrLn "Готово!"
