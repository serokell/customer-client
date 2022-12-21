-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: Apache-2.0

module Customer.Events.Types.TrackCustomerEvent
  ( TrackCustomerEventBody(..)
  , defaultTrackCustomerEvent
  ) where

import Customer.Aeson (defaultAesonOptions)
import Data.Aeson (Object)
import Data.Aeson.TH (deriveToJSON)
import Data.Text (Text)

data TrackCustomerEventBody = MkTrackCustomerEvent
  { tceName      :: Text
  , tceId        :: Maybe Text
  , tceTimestamp :: Maybe Int
  , tceData      :: Maybe Object
  }

deriveToJSON defaultAesonOptions ''TrackCustomerEventBody

defaultTrackCustomerEvent :: Text -> TrackCustomerEventBody
defaultTrackCustomerEvent name = MkTrackCustomerEvent name Nothing Nothing Nothing
