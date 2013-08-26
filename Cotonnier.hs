{-# LANGUAGE TypeFamilies, QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}

import Yesod.Static (StaticRoute, Route(StaticRoute), Static, static)
import Yesod (parseRoutes)
import Control.Applicative ((<$>), (<*>))
import Database.MongoDB ((=:))
import qualified Yesod as Yesod
import qualified Yesod.Static as Static
import qualified Mongo as Mongo
import qualified Database.MongoDB as MongoDB
import qualified Data.Text as Text
import qualified Yesod.Markdown as Markdown
import qualified Text.Pandoc as Pandoc
import qualified Yesod.Static as Static
import qualified Yesod as Yesod
import qualified Yesod.Core.Content as Content
import Mongo as Mongo


-- Basic definitions and routing informations for the application


data Cotonnier = Cotonnier { getStatic :: Static.Static }

Static.staticFiles "static"

Yesod.mkYesod "Cotonnier" [parseRoutes|
/ HomeR GET
/post/#Integer CotonsIdR GET POST
/post/#Integer/export.epub EpubR GET
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


-- Epub stuff


newtype RepEpub = RepEpub Content.Content

epubToContent = return . Content.toContent

epubToRepEpub = fmap RepEpub . epubToContent

typeEpub :: Content.ContentType
typeEpub = "application/epub+zip"

repEpub :: Content.ToContent a => a -> RepEpub
repEpub = RepEpub . Content.toContent


instance Content.HasContentType RepEpub where
      getContentType _ = typeEpub

deriving instance Content.ToContent RepEpub

instance Content.ToTypedContent RepEpub where
      toTypedContent (RepEpub c) = Content.TypedContent typeEpub c


-- Functions used by the handlers


readMarkdownFromFile id = do
  file <- Prelude.readFile (getStaticArticle id)
  let md = Pandoc.readMarkdown (Pandoc.def Pandoc.ReaderOptions) file
  epub <- Pandoc.writeEPUB (Pandoc.def Pandoc.WriterOptions) md
  return epub

createPage content =
  Yesod.defaultLayout $ do
    Yesod.setTitle "Cotonnier"
    Yesod.addStylesheet $ StaticR knacss_css
    Yesod.addStylesheet $ StaticR cotonnier_css
    content

commentForm = Yesod.renderDivs $ Comment
              <$> Yesod.areq Yesod.textField "Name" Nothing
              <*> Yesod.areq Yesod.textareaField "Content" Nothing

data Comment = Comment
               { name :: Text.Text
               , content :: Yesod.Textarea
               } deriving Show


-- The handlers themselves


getHomeR :: Handler Yesod.Html
getHomeR = do
  entries <- Yesod.liftIO $ Mongo.queryDocumentsWith [] "cotons" 10
  createPage $(Yesod.whamletFile "Home.hamlet")

getCotonsIdR :: Integer -> Handler Yesod.Html
getCotonsIdR id = do
  corpusmd <- Yesod.liftIO $ Markdown.markdownFromFile (getStaticArticle id)
  metadatas <- Yesod.liftIO $ Mongo.queryDocumentWith ["id" =: id] "cotons"
  comments <- Yesod.liftIO $ Mongo.queryDocumentsWith ["id" =: id] "com" 20
  (widget, enctype) <- Yesod.generateFormPost commentForm
  createPage $(Yesod.whamletFile "Post.hamlet")

postCotonsIdR :: Integer -> Handler Yesod.Html
postCotonsIdR id = do
  ((result, _), _) <- Yesod.runFormPost commentForm
  case result of -- Ignoring all errors
    Yesod.FormSuccess comment ->
        Yesod.liftIO $ Mongo.insertComment id (name comment) (content comment)
              >> return ()
    _ -> return ()
  getCotonsIdR id

getAuthorR :: String -> Handler Yesod.Html
getAuthorR author = do
  entries <- Yesod.liftIO
                    $ Mongo.queryDocumentsWith ["author" =: author] "cotons" 0
  createPage $(Yesod.whamletFile "Home.hamlet")

getTagsR :: String -> Handler Yesod.Html
getTagsR tag = do
  entries <- Yesod.liftIO $ Mongo.queryDocumentsWith ["tags" =: tag] "cotons" 0
  createPage $(Yesod.whamletFile "Home.hamlet")

getEpubR :: Integer -> Handler RepEpub
getEpubR id = do
  document <- Yesod.liftIO $ readMarkdownFromFile id
  (epubToRepEpub document)


-- Main function, launch the webserver and starts the application


main :: IO ()
main = do
  static@ (Static.Static settings) <- static "static"
  Yesod.warp 3000 $ Cotonnier static
