{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, ExtendedDefaultRules #-}

import Control.Applicative ((<$>), (<*>))
import Database.MongoDB ((=:))
import Yesod.Static (StaticRoute, Route(StaticRoute), Static, static)
import qualified Yesod as Yesod
import qualified Yesod.Static as Static
import qualified Mongo as Mongo
import qualified Yesod.Markdown as Markdown
import qualified Database.MongoDB as MongoDB
import qualified Data.Text as Text
import Yesod (parseRoutes)

data Cotonnier = Cotonnier { getStatic :: Static.Static }

Static.staticFiles "static"

Yesod.mkYesod "Cotonnier" [parseRoutes|
/ HomeR GET
/post/#Integer CotonsIdR GET POST
/author/#String AuthorR GET
/tags/#String TagsR GET
/static    StaticR Static getStatic
|]

instance Yesod.Yesod Cotonnier

instance Yesod.RenderMessage Cotonnier Yesod.FormMessage where
    renderMessage _ _ = Yesod.defaultFormMessage

getStaticArticle :: Show a => a -> String
getStaticArticle id = "/articles/paths/" ++
                      show id ++ "/article.md"

createPage content =
  Yesod.defaultLayout $ do
    Yesod.setTitle "Cotonnier"
    Yesod.addStylesheet $ StaticR knacss_css
    Yesod.addStylesheet $ StaticR cotonnier_css
    content

getHomeR :: Handler Yesod.RepHtml
getHomeR = do
  entries <- Yesod.liftIO $ Mongo.queryDocumentsWith [] "cotons" 10
  createPage $(Yesod.whamletFile "Home.hamlet")

data Comment = Comment
               { name :: Text.Text
               , content :: Text.Text
               } deriving Show

commentForm = Yesod.renderDivs $ Comment
              <$> Yesod.areq Yesod.textField "Name" Nothing
              <*> Yesod.areq Yesod.textField "Content" Nothing

getCotonsIdR :: Integer -> Handler Yesod.RepHtml
getCotonsIdR id = do
  corpusmd <- Yesod.liftIO $ Markdown.markdownFromFile (getStaticArticle id)
  metadatas <- Yesod.liftIO $ Mongo.queryDocumentWith ["id" =: id] "cotons"
  comments <- Yesod.liftIO $ Mongo.queryDocumentsWith ["id" =: id] "com" 20
  (widget, enctype) <- Yesod.generateFormPost commentForm
  createPage $(Yesod.whamletFile "Post.hamlet")

postCotonsIdR :: Integer -> Handler Yesod.RepHtml
postCotonsIdR id = do
  ((result, _), _) <- Yesod.runFormPost commentForm
  case result of -- Ignoring all errors
    Yesod.FormSuccess comment ->
        Yesod.liftIO $ Mongo.insertComment id (name comment) (content comment)
              >> return ()
    _ -> return ()
  getCotonsIdR id

getAuthorR :: String -> Handler Yesod.RepHtml
getAuthorR author = do
  entries <- Yesod.liftIO
                    $ Mongo.queryDocumentsWith ["author" =: author] "cotons" 0
  createPage $(Yesod.whamletFile "Home.hamlet")

getTagsR :: String -> Handler Yesod.RepHtml
getTagsR tag = do
  entries <- Yesod.liftIO $ Mongo.queryDocumentsWith ["tags" =: tag] "cotons" 0
  createPage $(Yesod.whamletFile "Home.hamlet")

main :: IO ()
main = do
  static@ (Static.Static settings) <- static "static"
  Yesod.warp 3000 $ Cotonnier static
