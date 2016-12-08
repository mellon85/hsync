{-# LANGUAGE TemplateHaskell #-}

module FSWatcherTest where

-- Symlink can be checked only on linu with the Unix package
import qualified FSWatcher as FS
import MyArbitrary

prop_test1 = assert True

return []
runTests = $quickCheckAll