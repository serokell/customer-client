-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: Apache-2.0

module Customer.Track.Events.Types.TrackCustomerEvent
  ( module Customer.Track.Events.Types.TrackCustomerEvent
  ) where

import Customer.Aeson (defaultAesonOptions, mkObject, mkPair)
import Data.Aeson (Object, ToJSON(toJSON), Value(..))
import Data.Aeson.TH (deriveToJSON)
import Data.Text (Text)

data TrackCustomerEventBody = MkTrackCustomerEvent
  { tceName      :: Text
  , tceId        :: Maybe Text
  , tceTimestamp :: Maybe Int
  , tceData      :: Maybe CustomerEventData
  }

data CustomerEventData = MkCustomerEventData
  { cedRecipient        :: Maybe Value
  , cedFromAddress      :: Maybe Value
  , cedReplyTo          :: Maybe Value
  , cedAdditionalFields :: Maybe Object
  }

instance ToJSON CustomerEventData where
  toJSON MkCustomerEventData{..} = case cedAdditionalFields of
    Just af -> Object $ mainFields <> af
    Nothing -> Object mainFields
    where
      mainFields = mkObject
        [ mkPair "recipient"    <$> cedRecipient
        , mkPair "from_address" <$> cedFromAddress
        , mkPair "reply_to"     <$> cedReplyTo
        ]

defaultTrackCustomerEvent
  :: Text -- ^ name
  -> TrackCustomerEventBody
defaultTrackCustomerEvent name = MkTrackCustomerEvent name Nothing Nothing Nothing

deriveToJSON defaultAesonOptions ''TrackCustomerEventBody
