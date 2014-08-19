{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Csv
import Data.Text
import qualified Data.Vector          as V

data CsvLine = CsvLine { stuff :: (V.Vector String) } deriving (Show)

instance FromRecord CsvLine where
	parseRecord v = CsvLine v


mkPer :: CsvLine -> [Char]
mkPer a = show a
--mkPer a = "Person {name: " ++ name a ++ ", salary: " ++ show (salary a) ++ "}"


main :: IO ()
main = do
    csvData <- BL.readFile "salaries.csv"
    case decode HasHeader csvData of
        Left err -> putStrLn err
        Right v  -> V.forM_ v $ (putStrLn . mkPer)


