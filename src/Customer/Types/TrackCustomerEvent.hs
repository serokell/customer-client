-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: Apache-2.0

module Customer.Types.TrackCustomerEvent
  ( TrackCustomerEventBody(..)
  , defaultTrackCustomerEvent
  ) where

import Customer.Aeson (defaultAesonOptions)
import Data.Aeson (Object, ToJSON(toJSON), genericToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data TrackCustomerEventBody = MkTrackCustomerEvent
  { tceName      :: Text
  , tceId        :: Maybe Text
  , tceTimestamp :: Maybe Int
  , tceData      :: Maybe Object
  }deriving stock (Generic)

instance ToJSON TrackCustomerEventBody where
  toJSON = genericToJSON (defaultAesonOptions "tce")

defaultTrackCustomerEvent :: Text -> TrackCustomerEventBody
defaultTrackCustomerEvent name = MkTrackCustomerEvent name Nothing Nothing Nothing
