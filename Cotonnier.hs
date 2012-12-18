{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, ExtendedDefaultRules #-}

import Yesod as Yesod
import Mongo as Mongo
import Yesod.Markdown as Markdown
import Yesod.Static

data Cotonnier = Cotonnier
  {
    getStatic	:: Static
  }

staticFiles "static"

mkYesod "Cotonnier" [parseRoutesNoCheck|
/ HomeR GET
/#Integer CotonsIdR GET
/static    StaticR Static getStatic
|]

instance Yesod Cotonnier

getStaticArticle id = "/home/engil/static/" ++ (show id) ++ "/article.md"

getHomeR :: Handler Yesod.RepHtml
getHomeR = Yesod.defaultLayout [whamlet|Accueil|]

getCotonsIdR :: Integer -> Handler Yesod.RepHtml
getCotonsIdR id = do
  query <- Yesod.liftIO $ Mongo.queryMetadataById id
  corpusmd <- Yesod.liftIO $ Markdown.markdownFromFile (getStaticArticle id)
  let metadatas = query
      author = Mongo.getString $ getValue metadatas "author"
      tags = Mongo.getList $ getValue metadatas "tags"
      date = Mongo.getDate $ getValue  metadatas "date"
      title = Mongo.getString $ getValue  metadatas "title"
      corpus = Markdown.markdownToHtml corpusmd
  Yesod.defaultLayout $ do
    setTitle "Cotonnier"
    addStylesheet $ StaticR knacss_css
    addStylesheet $ StaticR cotonnier_css
    $(Yesod.whamletFile "Post.hamlet")

main :: IO ()
main = do
  static@ (Static settings) <- static "static"
  Yesod.warpDebug 3000 $ Cotonnier static
