{-# LANGUAGE OverloadedStrings #-}

import Pipes
import System.IO

import Pipes.Csv (decode, decodeByName)
import Prelude hiding (readFile, withFile)
import qualified Pipes.ByteString as P
import Data.Csv ((.:), FromNamedRecord(..), Record)
import Pipes
import Control.Applicative

data Person = Person String Int Int String
            deriving (Show)

instance FromNamedRecord Person where
  parseNamedRecord p =
    Person <$> p .: "name"
           <*> p .: "salary"
           <*> p .: "age"
           <*> p .: "location"


persons :: Monad m
        => Producer P.ByteString m ()
        -> Producer (Either String Person) m ()
persons = decodeByName

main = 
	withFile "salaries.csv" ReadMode $ \input -> 
		runEffect $ for (persons (P.fromHandle input)) (lift . print)