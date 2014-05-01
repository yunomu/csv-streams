module Main where

import Test.Hspec
import System.IO.Streams
import System.IO.Streams.CSV

main :: IO ()
main = hspec $ do
    describe "System.IO.Streams.CSV" $ do
        it "parse CSV." testToCSV
        it "parse CSV stream." testToCSVRow

testToCSV :: IO ()
testToCSV = do
    is <- fromByteString "abc,def,ghi\njkl,mno,pqr"
    a <- toCSV is
    a `shouldBe` [["abc","def","ghi"],["jkl","mno","pqr"]]

testToCSVRow :: IO ()
testToCSVRow = do
    is <- fromByteString "abc,def,ghi\njkl,mno,pqr"
    rs <- toCSVRow is
    a <- toList rs
    print a
--    print a
--    a `shouldBe` [["abc","def","ghi"],["jkl","mno","pqr"]]
