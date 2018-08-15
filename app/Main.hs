{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveGeneric #-}
module Main where

import Control.Applicative ((<$>), optional)
import Control.Monad (msum)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import GHC.Generics
import Happstack.Server
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


data Photo =
  Photo { albumId :: Int
        , id :: Int
        , title :: String
        , url :: String
        , thumbnailUrl :: String
        } deriving (Show, Generic)


instance FromJSON Photo
instance ToJSON Photo


db :: FilePath
db = "db.json"


loadDB :: IO B.ByteString -> IO ()
loadDB = B.readFile db


main :: IO ()
main = simpleHTTP nullConf myApp


myApp :: ServerPart Response
myApp = msum
    [ dir "data" $ dataPage
    , dir "404" $ fileServing
    , dir "new" $ postNewData
    , homePage
    ]

template :: Text -> Html -> Response
template title body = toResponse $
  H.html $ do
    H.head $ do
      H.title (toHtml title)
    H.body $ do
      body
      p $ a ! href "/" $ "back home"


homePage :: ServerPart Response
homePage =
  ok $ template "Home page" $ do
    H.h1 "Welcome!"
    H.p "A paragraph to show it works."
    H.p $ a ! href "/upload" $ "non-working link"


dataPage :: ServerPart Response
dataPage =
  path $ \(msg :: String) ->
  ok $ template "Data" $ do
    p $ "You wanted the page data with " >> toHtml msg
    p "... done."


fileServing :: ServerPart Response
fileServing =
  serveDirectory EnableBrowsing ["404.html"] "html"


postNewData :: ServerPart Response
postNewData =
  do firstName <- look "firstName"
     lastName <- look "lastName"
     let
       msg = "Hello " ++ firstName ++ " " ++ lastName
        in ok $ toResponse msg
