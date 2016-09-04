import DB 


main :: IO ()
main = do
    DB.runTests
    return ()
