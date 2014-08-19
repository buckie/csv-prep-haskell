{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Lazy as BL
import           Data.Csv
import Data.Text
import qualified Data.Vector          as V

p :: V.Vector [Char] -> IO ()
p csvLine = putStrLn $ "Time: " ++ (csvLine V.! 0)

main :: IO ()
main = do
    csvData <- BL.readFile "sample_data.csv"
    case decode HasHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v p

--instance FromNamedRecord Person where
--    parseNamedRecord r = Person <$> r .: "name" <*> r .: "salary"
--
--main = do
    --csvData <- BL.readFile "salaries.csv"
    --case decodeByName csvData of
    --    Left err -> putStrLn err
    --    Right (_, v)  -> V.forM_ v $ \ p ->
    --        putStrLn $ name p ++ " earns " ++ show (salary p) ++ " dollars"

