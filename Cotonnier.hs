{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, ExtendedDefaultRules #-}

import Yesod as Yesod

data Cotonnier = Cotonnier

mkYesod "Cotonnier" [parseRoutes|
/ HomeR GET
/cotons CotonsR GET
/cotons/id/#Int CotonsIdR GET
|]

instance Yesod Cotonnier

getHomeR :: Handler Yesod.RepHtml
getHomeR = Yesod.defaultLayout [whamlet|Accueil|]

getCotonsR :: Handler Yesod.RepHtml
getCotonsR = Yesod.defaultLayout [whamlet|Accueil|]

getCotonsIdR :: Int -> Handler Yesod.RepHtml
getCotonsIdR id = Yesod.defaultLayout $(Yesod.whamletFile "Home.hamlet")
                                                              where title = "title"

main :: IO ()
main = Yesod.warpDebug 3000 Cotonnier
