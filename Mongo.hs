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

import Database.MongoDB ((=:), Field((:=)))
import qualified Database.MongoDB as DB
import qualified Data.Maybe as Maybe
import qualified System.Locale as Locale
import qualified Data.Time as Time
import qualified Data.Time.Format as Format
import qualified GHC.Word as Word
import qualified Data.Text as Text
import qualified Yesod as Yesod

initMongoCo :: IO DB.Pipe
initMongoCo = DB.runIOE $ DB.connect $ DB.host "127.0.0.1"

mongoRun :: DB.Pipe -> DB.Action IO a -> IO (Either DB.Failure a)
mongoRun pipe = DB.access pipe DB.master "cotonnier"

valueToString :: Maybe String -> String
valueToString (Just a) = a
valueToString Nothing = "Error xd"

getId :: DB.Document -> DB.Label -> Integer
getId = getValue 0 id

getDate :: DB.Document -> DB.Label -> String
getDate =
    let f = Format.formatTime Locale.defaultTimeLocale in
    getValue "Value error" $ \x -> f "%A %e, %B %Y" (x :: Time.UTCTime)

getString :: DB.Document -> DB.Label -> String
getString = getValue "Value error" id

getList :: DB.Document -> DB.Label -> [String]
getList = getValue ["Value error"] id

getValue :: DB.Val a => b -> (a -> b) -> DB.Document -> DB.Label -> b
getValue defaultValue f document field =
    Maybe.maybe defaultValue f $ DB.look field document >>= DB.cast'

queryDocumentWith :: DB.Selector ->
                     DB.Collection ->
                     IO (Either String DB.Document)
queryDocumentWith query collection = do
  pipe <- initMongoCo
  response <- mongoRun pipe $
              DB.findOne $
              DB.select query collection
  return $ checkResponse response

queryDocumentsWith :: [DB.Field] ->
                      Text.Text ->
                      Word.Word32 ->
                      IO (Either DB.Failure [DB.Document])
queryDocumentsWith query collection limitation = do
  pipe <- initMongoCo
  let modifier x = x {DB.limit = limitation, DB.sort = ["id" =: -1]}
      find_cotons = DB.find $ modifier $ DB.select query collection
  mongoRun pipe $ find_cotons >>= DB.rest

checkResponse :: DB.Val a => Either DB.Failure (Maybe a) -> Either String a
checkResponse (Left _) = Left "Error while querying DB"
checkResponse (Right Nothing) = Left "Querying return nothing"
checkResponse (Right (Just a)) = Right a

insertComment :: DB.Val a =>
                 a ->
                 Text.Text ->
                 Yesod.Textarea ->
                 IO (Either DB.Failure DB.Value)
insertComment id' author content = do
  pipe <- initMongoCo
  time <- Time.getCurrentTime
  mongoRun pipe $ DB.insert "com"
    [ "id" := DB.val id'
    , "date" := DB.UTC time
    , "author" := DB.String author
    , "content" := DB.String (Yesod.unTextarea content)
    ]
