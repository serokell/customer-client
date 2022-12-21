-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RecordWildCards #-}

module Customer
  ( module Customer
  , ReportPushMetricsBody(..)
  , TrackAnonymusEventBody(..)
  , TrackCustomerEventBody(..)
  ) where

import Customer.Events.API (api)
import Customer.Events.Types.ReportPushMetrics (ReportPushMetricsBody(..), defaultReportPushMetrics)
import Customer.Events.Types.TrackAnonymusEvent (TrackAnonymusEventBody(..), defaultTrackAnonymusEvent)
import Customer.Events.Types.TrackCustomerEvent (TrackCustomerEventBody(..), defaultTrackCustomerEvent)
import Data.Text (Text)
import qualified Network.HTTP.Client as HTTP
import Servant.API
import Servant.Client

host :: BaseUrl
host = BaseUrl Https "track.customer.io" 443 ""

data Env = MkEnv
  { authtoken :: BasicAuthData
  , clientEnv :: ClientEnv
  }

mkEnv :: BasicAuthData -> HTTP.Manager -> Env
mkEnv authtoken httpManager = MkEnv {..}
  where
    clientEnv = mkClientEnv httpManager host

trackCustomerEventC :: BasicAuthData -> Text -> TrackCustomerEventBody -> ClientM ()
trackAnonymusEventC :: BasicAuthData -> TrackAnonymusEventBody -> ClientM ()
reportPushMetricsC  :: ReportPushMetricsBody -> ClientM ()

trackCustomerEventC
  :<|> trackAnonymusEventC
  :<|> reportPushMetricsC
  = client api

trackCustomerEventC' :: Env -> Text -> TrackCustomerEventBody -> IO (Either ClientError ())
trackCustomerEventC' MkEnv{..} identifier body = do
  runClientM (trackCustomerEventC authtoken identifier body) clientEnv

trackAnonymusEventC' :: Env -> TrackAnonymusEventBody -> IO (Either ClientError ())
trackAnonymusEventC' MkEnv{..} body = do
  runClientM (trackAnonymusEventC authtoken body) clientEnv

reportPushMetricsC' :: Env -> ReportPushMetricsBody -> IO (Either ClientError ())
reportPushMetricsC' MkEnv{..} body = do
  runClientM (reportPushMetricsC body) clientEnv

trackCustomerEvent :: Env -> Text -> Text -> IO (Either ClientError ())
trackCustomerEvent env identifier = trackCustomerEventC' env identifier . defaultTrackCustomerEvent

trackAnonymusEvent :: Env -> Text -> IO (Either ClientError ())
trackAnonymusEvent env = trackAnonymusEventC' env . defaultTrackAnonymusEvent

reportPushMetrics :: Env -> IO (Either ClientError ())
reportPushMetrics env = reportPushMetricsC' env defaultReportPushMetrics
