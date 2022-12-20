-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: Apache-2.0

module Customer.Types.TrackAnonymusEvent
  ( TrackAnonymusEventBody(..)
  , defaultTrackAnonymusEvent
  ) where

import Customer.Aeson (defaultAesonOptions)
import Data.Aeson (Object, ToJSON(toJSON), genericToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data TrackAnonymusEventBody = MkTrackAnonymusEvent
  { taeName        :: Text
  , taeAnonymousId :: Maybe Text
  , taeId          :: Maybe Text
  , taeTimestamp   :: Maybe Int
  , taeData        :: Maybe Object
  } deriving stock (Generic)

instance ToJSON TrackAnonymusEventBody where
  toJSON = genericToJSON (defaultAesonOptions "tae")

defaultTrackAnonymusEvent :: Text -> TrackAnonymusEventBody
defaultTrackAnonymusEvent name = MkTrackAnonymusEvent name Nothing Nothing Nothing Nothing
