module Lib where


import Data.Aeson
-- import Data.ByteString.Lazy.readFile
import Data.Maybe (fromMaybe)
import Data.Text.Lazy.IO as T
import Data.Text.Lazy.Encoding as T
import Data.Time.ISO8601 (parseISO8601)
import Data.Time.Format (formatTime)
import System.Locale (iso8601DateFormat, rfc822DateFormat)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import GHC.Generics

data Photo =
  Photo { albumId :: Int
        , id :: Int
        , title :: String
        , url :: String
        , thumbnailUrl :: String
        } deriving (Show, Generic)


instance FromJSON Photo
instance ToJSON Photo


db :: String
db = "db.json"

loadDB :: IO B.ByteString
loadDB = B.readFile db

saveDB :: B.ByteString -> IO ()
saveDB state =
      B.writeFile "saved_db.json" state

main =
  do
    record <- (eitherDecode <$> loadDB) :: IO (Either String [Photo])
    case record of
      Left err -> Prelude.putStrLn err
      Right rec ->
        saveDB $ encode rec
