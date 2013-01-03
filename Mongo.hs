{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Mongo(
  initMongoCo
  , mongoRun
  , valueToString
  , queryDocumentWith
  , queryDocumentsWith
  , insertComment
  , checkResponse
  , getString
  , getValue
  , getDate
  , getList
  , getId
  ) where

import Database.MongoDB as MongoDB
import Data.Maybe as Maybe
import Control.Monad.Trans (liftIO)
import System.Locale as Locale
import Data.Time
import Data.Time.Format as Format

initMongoCo = runIOE $ MongoDB.connect $ MongoDB.host "127.0.0.1"

mongoRun pipe = MongoDB.access pipe MongoDB.master "cotonnier"

valueToString :: Maybe String -> String
valueToString (Just a) = a
valueToString Nothing = "Error xd"

getId :: Either String (Maybe Integer) -> Integer
getId (Left error) = 0
getId (Right (Just a)) = a
getId (Right Nothing) = 0

getDate :: Either String (Maybe UTCTime) -> String
getDate (Left error) = error
getDate (Right Nothing) = "Value error"
getDate (Right (Just date)) =
    Format.formatTime Locale.defaultTimeLocale "%A %e, %B %Y" date

getString :: Either String (Maybe String) -> String
getString (Left error) = error
getString (Right Nothing) = "Value error"
getString (Right (Just value)) = value

getList :: Either String (Maybe [String]) -> [String]
getList (Left error) = [error]
getList (Right Nothing) = ["Value error"]
getList (Right (Just list)) = list

getValue :: (Val a) => Either String Document -> Label -> Either String (Maybe a)
getValue (Left error) _ = Left error
getValue (Right document) field =
    Right $ MongoDB.look field document >>= MongoDB.cast'

queryDocumentWith :: Selector -> Collection -> IO (Either String Document)
queryDocumentWith query collection = do
  pipe <- initMongoCo
  response <- mongoRun pipe $ 
              MongoDB.findOne $ 
              MongoDB.select query collection
  let document = checkResponse response
  return document

queryDocumentsWith query collection limitation = do
  pipe <- initMongoCo
  let modifier x = x {limit = limitation, sort = ["id" =: -1]}
      find_cotons = MongoDB.find $ modifier $ MongoDB.select query collection
  mongoRun pipe $ find_cotons >>= MongoDB.rest

checkResponse :: Val a => Either Failure (Maybe a) -> Either String a
checkResponse (Left _) = Left "Error while querying MongoDB"
checkResponse (Right Nothing) = Left "Querying return nothing"
checkResponse (Right (Just a)) = Right a

insertComment id' author content = do
  pipe <- initMongoCo
  time <- getCurrentTime
  mongoRun pipe $ MongoDB.insert "com" 
    [ "id" := val id'
    , "date" := UTC time
    , "author" := String author
    , "content" := String content
    ]
