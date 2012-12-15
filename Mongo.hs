{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Mongo(
initMongoCo
, mongoRun
, lookupData
, valueToString
, queryMetadataById
, checkResponse
, getString
, getValue
) where

import Database.MongoDB as MongoDB
import Data.Maybe as Maybe

initMongoCo = runIOE $ MongoDB.connect $ MongoDB.host "127.0.0.1"

mongoRun pipe act = MongoDB.access pipe MongoDB.master "cotonnier" act

valueToString :: Maybe String -> String
valueToString (Just a) = a
valueToString Nothing = "Error xd"

lookupData field doc = MongoDB.look field doc

getString :: Either String (Maybe String) -> String
getString (Left error) = error
getString (Right (Nothing)) = "Value error"
getString (Right (Just value)) = value

getValue :: (Val a) => Either String Document -> Label -> Either String (Maybe a)
getValue (Left error) _ = Left error
getValue (Right document) field = Right val where val = getval $ lookupData field document
                                                  getval Nothing = Nothing
                                                  getval (Just a) = MongoDB.cast' a

queryMetadataById :: Integer -> IO (Either String Document)
queryMetadataById id = do
  pipe <- initMongoCo
  response <- mongoRun pipe $ MongoDB.findOne $ MongoDB.select ["id" =: id] "cotons"
  let document = checkResponse response
  return document

checkResponse :: (Either Failure (Maybe Document)) -> (Either String Document)
checkResponse (Left _) = Left "Error while querying MongoDB"
checkResponse (Right Nothing) = Left "Querying return nothing"
checkResponse (Right (Just document)) = Right document
