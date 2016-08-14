module Main where

import Lib

main :: IO ()
main = do
    {- v <- test_ffi 0
	print v 
	v <- test_ffi 0
	print v
    -}
    stopDHT
    print "moo"
