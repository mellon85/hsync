import qualified DBtest
import qualified FSWatcherTest

main :: IO ()
main = do
    DBtest.runTests
    FSWatcherTest.runTests
    return ()
