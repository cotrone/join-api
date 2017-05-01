{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Join (
    APIKey(..)
  , DeviceId(..)
  , SMS(..)
  , sendSMS
  , sendPush
  ) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.String
import Network.URL
import Network.Wreq
import Data.ByteString.Lazy (ByteString)
import Control.Lens

-- | API Key for the account
newtype APIKey = APIKey {
  unAPIKey :: Text
} deriving (Eq, Ord, Show, IsString)

-- | Device ID to send the message to/through
newtype DeviceId = DeviceId {
  unDeviceId :: Text
} deriving (Eq, Ord, Show, IsString)

-- | SMS Message with phone number to send the
-- message to
data SMS = SMS {
  smsNumber :: Text
, smsText :: Text
} deriving (Eq, Ord, Show)

joinUrlType :: URLType
joinUrlType =
  Absolute $
    Host (HTTP True) "joinjoaomgcd.appspot.com" Nothing

joinPath :: String
joinPath = "_ah/api/messaging/v1/sendPush"

data JoinResponse = JoinResponse {
  joinSuccess :: Bool
, joinUserAuthError :: Bool
, joinKind :: Text
, joinETag :: Text
}

instance FromJSON JoinResponse where
  parseJSON (Object o) =
    JoinResponse
      <$> (o .: "success")
      <*> (o .: "userAuthError")
      <*> (o .: "kind")
      <*> (o .: "etag")

-- | Send an SMS from the given device.
-- If the device doesn't allow join to send SMS,
-- there will be an error in the response
sendSMS :: APIKey -> DeviceId -> SMS -> IO JoinResponse
sendSMS (APIKey key) (DeviceId i) (SMS number msg) = do
  resp <- asJSON =<< (get $ exportURL smsUrl)
  return $ resp ^. responseBody
  where
    smsUrl = URL joinUrlType joinPath params
    params = (fmap T.unpack) <$> [
        ("deviceId", i)
      , ("apikey", key)
      , ("smsnumber", number)
      , ("smstext", msg)
      ]

-- | Send a push notification to a given device
sendPush :: APIKey -> DeviceId -> Text -> IO JoinResponse
sendPush (APIKey key) (DeviceId i) msg = do
  resp <- asJSON =<< (get $ exportURL smsUrl)
  return $ resp ^. responseBody
  where
    smsUrl = URL joinUrlType joinPath params
    params = (fmap T.unpack) <$> [
        ("deviceId", i)
      , ("apikey", key)
      , ("text", msg)
      ]