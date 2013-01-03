{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, ExtendedDefaultRules #-}

import Yesod as Yesod
import Mongo as Mongo
import Yesod.Markdown as Markdown
import Database.MongoDB as MongoDB
import Yesod.Static

data Cotonnier = Cotonnier { getStatic :: Static }

staticFiles "static"

mkYesod "Cotonnier" [parseRoutesNoCheck|
/ HomeR GET
/#Integer CotonsIdR GET
/author/#String AuthorR GET
/tags/#String TagsR GET
/static    StaticR Static getStatic
|]

instance Yesod Cotonnier

getStaticArticle id = "/home/thomas/dev/Haskell/Cotonnier/articles/" ++
                      show id ++ "/article.md"

createPage :: GWidget Cotonnier Cotonnier () -> Handler Yesod.RepHtml
createPage content =
  Yesod.defaultLayout $ do
    setTitle "Cotonnier"
    addStylesheet $ StaticR knacss_css
    addStylesheet $ StaticR cotonnier_css
    content

getHomeR :: Handler Yesod.RepHtml
getHomeR = do
  entries <- Yesod.liftIO $ Mongo.queryDocumentsWith [] "cotons" 10
  createPage $(Yesod.whamletFile "Home.hamlet")

getCotonsIdR :: Integer -> Handler Yesod.RepHtml
getCotonsIdR id = do
  metadatas <- Yesod.liftIO $ Mongo.queryDocumentWith ["id" =: id] "cotons"
  comments <- Yesod.liftIO $ Mongo.queryDocumentsWith ["id" =: id] "com" 20
  corpusmd <- Yesod.liftIO $ Markdown.markdownFromFile (getStaticArticle id)
  createPage $(Yesod.whamletFile "Post.hamlet")

getAuthorR :: String -> Handler Yesod.RepHtml
getAuthorR author = do
  entries <- Yesod.liftIO $ queryDocumentsWith ["author" =: author] "cotons" 0
  createPage $(Yesod.whamletFile "Home.hamlet")

getTagsR :: String -> Handler Yesod.RepHtml
getTagsR tag = do
  entries <- Yesod.liftIO $ queryDocumentsWith ["tags" =: tag] "cotons" 0
  createPage $(Yesod.whamletFile "Home.hamlet")

main :: IO ()
main = do
  static@ (Static settings) <- static "static"
  Yesod.warpDebug 3000 $ Cotonnier static
