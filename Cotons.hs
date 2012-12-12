{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Cotons (getCotonById) where

import Mongo as Mongo
import Data.IO.Unfafe as Unsafe

getCotonById id = $(Yesod.whamletFile "Post.hamlet") where
                                                  metadatas = unsafePerformIO $ Mongo.queryMetadataById id
                                                  author = Mongo.getValue metadatas "author"
                                                  tags = Mongo.getValue metadatas "tags"
                                                  date = Mongo.getValue metadatas "date"
                                                  title = Mongo.getValue metadatas "title"
                                                  corpus = Mongo.getArticle id
