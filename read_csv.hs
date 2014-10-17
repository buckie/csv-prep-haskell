{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString.Char8
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

data WVal = WValB ByteString | WValI Int
	deriving (Eq, Show)

mkWValB :: ByteString -> WVal
mkWValB x = WValB x

mkWValI :: Int -> WVal
mkWValI x = WValI x

type WFieldResult = Either

wParseLine :: [WFieldParser] -> [ByteString] -> [Either [Char] WVal]
wParseLine _ [] = []
wParseLine [] _ = []
wParseLine (x:xs) (y:ys) = (wParseField x y) : wParseLine xs ys

wParseField :: WFieldParser -> ByteString -> Either [Char] WVal
wParseField (WFieldParser wfname WChar) y = r
	where
		r = case parseOnly nonNullText y of
				Left z -> Left $ "Parse Failure on " ++ wfname ++ ": " ++ z
				Right z -> Right $ mkWValB z
wParseField (WFieldParser wfname WInt) y = r
	where
		r = case parseOnly decimal y of
				Left z -> Left $ "Parse Failure on " ++ wfname ++ ": " ++ z
				Right z -> Right $ mkWValI z

nonNullText :: Parser ByteString
nonNullText = takeWhile1 isPrint

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
    return parsed
    -- return $ fmap (wParseLine wParserSpec) parsed

