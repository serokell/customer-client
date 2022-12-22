-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}

#if MIN_VERSION_GLASGOW_HASKELL(9,2,1,0)
{-# LANGUAGE NoFieldSelectors #-}
#endif

{-# OPTIONS_GHC -Wno-partial-fields #-}

module Customer.Track.Events.Types.TrackAnonymusEvent
  ( module Customer.Track.Events.Types.TrackAnonymusEvent
  ) where

import Customer.Aeson (defaultAesonOptions, mkObject, mkPair)
import Data.Aeson
import Data.Aeson.TH (deriveToJSON)
import Data.Text (Text)

data TrackAnonymusEventBody
  = MkStandardAnonymusEvent
    { saeName        :: Text
    , saeAnonymousId :: Maybe Text
    , saeId          :: Maybe Text
    , saeTimestamp   :: Maybe Int
    , saeData        :: Maybe Object
    }
  | MkInviteAnonymusEvent
    { iaeName      :: Text
    , iaeData      :: InviteAnonymusData
    , iaeTimestamp :: Maybe Int
    }

data StandardAnonymusData = MkStandardAnonymusData
  { sadFromAddress      :: Maybe Text
  , sadReplyTo          :: Maybe Text
  , sadAdditionalFields :: Maybe Object
  }

instance ToJSON StandardAnonymusData where
  toJSON MkStandardAnonymusData{..} = case sadAdditionalFields of
    Just km -> Object (mainFields <> km)
    Nothing -> Object mainFields
    where
      mainFields = mkObject
        [ mkPair "from_address" <$> sadFromAddress
        , mkPair "reply_to" <$> sadReplyTo
        ]

data InviteAnonymusData = MkInviteAnonymusData
  { iadRecipient        :: Text
  , iadFromAddress      :: Maybe Text
  , iadReplyTo          :: Maybe Text
  , iadAdditionalFields :: Maybe Object
  }

instance ToJSON InviteAnonymusData where
  toJSON MkInviteAnonymusData{..} = case iadAdditionalFields of
    Just km -> Object (mainFields <> km)
    Nothing -> Object mainFields
    where
      mainFields = mkObject
        [ Just (mkPair "recipient" iadRecipient)
        , mkPair "from_address" <$> iadFromAddress
        , mkPair "reply_to" <$> iadReplyTo
        ]

deriveToJSON (defaultAesonOptions {sumEncoding = UntaggedValue}) ''TrackAnonymusEventBody
