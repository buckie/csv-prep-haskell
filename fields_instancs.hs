{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Csv
import Data.Text
import qualified Data.Vector          as V

data Person = Person { name :: !String, salary :: !Int }

instance FromRecord Person where
	parseRecord v = Person <$> v .! 0 <*> v .! 1


mkPer :: Person -> [Char]
mkPer a = "Person {name: " ++ name a ++ ", salary: " ++ show (salary a) ++ "}"


main :: IO ()
main = do
    csvData <- BL.readFile "salaries.csv"
    case decode HasHeader csvData of
        Left err -> putStrLn err
        Right v  -> V.forM_ v $ (putStrLn . mkPer)


