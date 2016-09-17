import qualified DBtest


main :: IO ()
main = do
    DBtest.runTests
    return ()
