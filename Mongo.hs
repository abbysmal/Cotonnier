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
import GHC.Word (Word32)
import Data.Text (Text)

initMongoCo :: IO Pipe
initMongoCo = runIOE $ MongoDB.connect $ MongoDB.host "127.0.0.1"

mongoRun :: Pipe -> Action IO a -> IO (Either Failure a)
mongoRun pipe = MongoDB.access pipe MongoDB.master "cotonnier"

valueToString :: Maybe String -> String
valueToString (Just a) = a
valueToString Nothing = "Error xd"

getId :: Document -> Label -> Integer
getId = getValue 0 id

getDate :: Document -> Label -> String
getDate =
    let f = Format.formatTime Locale.defaultTimeLocale in
    getValue "Value error" $ \x -> f "%A %e, %B %Y" (x :: UTCTime)

getString :: Document -> Label -> String
getString = getValue "Value error" id

getList :: Document -> Label -> [String]
getList = getValue ["Value error"] id

getValue :: Val a => b -> (a -> b) -> Document -> Label -> b
getValue defaultValue f document field =
    Maybe.maybe defaultValue f $ MongoDB.look field document >>= MongoDB.cast'

queryDocumentWith :: Selector -> Collection -> IO (Either String Document)
queryDocumentWith query collection = do
  pipe <- initMongoCo
  response <- mongoRun pipe $
              MongoDB.findOne $
              MongoDB.select query collection
  let document = checkResponse response
  return document

queryDocumentsWith :: [Field] -> Text -> Word32 -> IO (Either Failure [Document])
queryDocumentsWith query collection limitation = do
  pipe <- initMongoCo
  let modifier x = x {limit = limitation, sort = ["id" =: -1]}
      find_cotons = MongoDB.find $ modifier $ MongoDB.select query collection
  mongoRun pipe $ find_cotons >>= MongoDB.rest

checkResponse :: Val a => Either Failure (Maybe a) -> Either String a
checkResponse (Left _) = Left "Error while querying MongoDB"
checkResponse (Right Nothing) = Left "Querying return nothing"
checkResponse (Right (Just a)) = Right a

insertComment :: Val a => a -> Text -> Text -> IO (Either Failure Value)
insertComment id' author content = do
  pipe <- initMongoCo
  time <- getCurrentTime
  mongoRun pipe $ MongoDB.insert "com"
    [ "id" := val id'
    , "date" := UTC time
    , "author" := String author
    , "content" := String content
    ]
