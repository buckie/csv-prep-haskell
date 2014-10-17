{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative
import           Control.Monad
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.ByteString.Lazy as ABL
import qualified Data.Attoparsec.ByteString.Char8 as AB8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (isPrint)
import qualified Data.Csv                         as CSV
import           Data.Text                        as T
import qualified Data.Vector                      as V
import           Prelude                          hiding (takeWhile1)

data WFieldType = WChar | WInt
	deriving (Eq, Show)

data WFieldParser = WFieldParser WFieldName WFieldType
	deriving (Eq, Show)

type WFieldName = [Char]

type WLineParser = [WFieldParser]

data WVal = WValB B8.ByteString | WValI Int
	deriving (Eq, Show)

mkWValB :: B8.ByteString -> WVal
mkWValB x = WValB x

mkWValI :: Int -> WVal
mkWValI x = WValI x

type WFieldResult = Either

wParseLine :: [WFieldParser] -> [BL.ByteString] -> [Either [Char] WVal]
wParseLine _ [] = []
wParseLine [] _ = []
wParseLine (x:xs) (y:ys) = (wParseField x y) : wParseLine xs ys

wParseField :: WFieldParser -> BL.ByteString -> Either [Char] WVal
wParseField (WFieldParser wfname WChar) y = r
	where
		r = case (ABL.eitherResult . (ABL.parse nonNullText) $ y) of
				Left z -> Left $ "Parse Failure on " ++ wfname ++ ": " ++ z
				Right z -> Right $ mkWValB z
wParseField (WFieldParser wfname WInt) y = r
	where
		r = case (ABL.eitherResult . (ABL.parse AB8.decimal) $ y) of
				Left z -> Left $ "Parse Failure on " ++ wfname ++ ": " ++ z
				Right z -> Right $ mkWValI z

nonNullText :: AB8.Parser B8.ByteString
nonNullText = AB8.takeWhile1 isPrint

fieldNames :: [[Char]]
fieldNames = ["name","salary","age","loc"]

fieldTypes :: [WFieldType]
fieldTypes = [WChar,WInt,WInt,WChar]

wParserSpec :: WLineParser
wParserSpec = fmap (\(x,y) -> WFieldParser x y) $ Prelude.zip fieldNames fieldTypes

runCsvParser :: Monad m => BL.ByteString -> m (V.Vector [BL.ByteString])
runCsvParser csvData = do
		case CSV.decode CSV.HasHeader csvData of
	        	Left err -> error $ show err
	        	Right v -> return (v :: V.Vector [BL.ByteString])

main = do
    csvData <- BL.readFile "salaries_locs.csv"
    parsed <- runCsvParser csvData
    return $ fmap (wParseLine wParserSpec) parsed

