{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Mongo
( initMongoCo
, mongoRun
, lookupData
, lookupInt
, valueToString
, queryMetadataById
, checkResponse
) where

import Database.MongoDB as MongoDB
import Data.Maybe as Maybe
import Data.String

initMongoCo = runIOE $ MongoDB.connect $ MongoDB.host "127.0.0.1"

mongoRun pipe act = MongoDB.access pipe MongoDB.master "cotonnier" act

valueToString :: Maybe String -> String
valueToString (Just a) = a
valueToString Nothing = "Error xd"


lookupData field doc = (MongoDB.look field doc >>=)

lookupInt :: a -> IO a
lookupInt a = return a

getValue :: Either String Document -> String
getValue (Left error) = error
getValue (Right document) =

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
