-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: Apache-2.0

module Customer.Events.Types.TrackAnonymusEvent
  ( TrackAnonymusEventBody(..)
  , defaultTrackAnonymusEvent
  ) where

import Customer.Aeson (defaultAesonOptions)
import Data.Aeson (Object)
import Data.Aeson.TH (deriveToJSON)
import Data.Text (Text)

data TrackAnonymusEventBody = MkTrackAnonymusEvent
  { taeName        :: Text
  , taeAnonymousId :: Maybe Text
  , taeId          :: Maybe Text
  , taeTimestamp   :: Maybe Int
  , taeData        :: Maybe Object
  }

deriveToJSON defaultAesonOptions ''TrackAnonymusEventBody

defaultTrackAnonymusEvent :: Text -> TrackAnonymusEventBody
defaultTrackAnonymusEvent name = MkTrackAnonymusEvent name Nothing Nothing Nothing Nothing
