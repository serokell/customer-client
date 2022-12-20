-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: Apache-2.0

module Customer.Types.ReportPushMetrics
  ( ReportPushMetricsBody(..)
  , defaultReportPushMetrics
  ) where

import Customer.Aeson (defaultAesonOptions)
import Data.Aeson (ToJSON(toJSON), genericToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data ReportPushMetricsBody = MkReportPushMetrics
  { rpmDeliveryId :: Maybe Text
  , rpmEvent      :: Maybe Text
  , rpmDeviceId   :: Maybe Text
  , rpmTimestamp  :: Maybe Int
  } deriving stock (Generic)

instance ToJSON ReportPushMetricsBody where
  toJSON = genericToJSON (defaultAesonOptions "rpm")

defaultReportPushMetrics :: ReportPushMetricsBody
defaultReportPushMetrics = MkReportPushMetrics Nothing Nothing Nothing Nothing
