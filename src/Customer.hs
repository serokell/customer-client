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

import Customer.Track.Events.API (api)
import Customer.Track.Events.Types.ReportPushMetrics
  (ReportPushMetricsBody(..), defaultReportPushMetrics)
import Customer.Track.Events.Types.TrackAnonymusEvent (TrackAnonymusEventBody(..))
import Customer.Track.Events.Types.TrackCustomerEvent
  (TrackCustomerEventBody(..), defaultTrackCustomerEvent)
import Data.Text (Text)
import qualified Network.HTTP.Client as HTTP
import Servant.API
import Servant.Client

-- | Default host of the track API: https://track.customer.io:443
host :: BaseUrl
host = BaseUrl Https "track.customer.io" 443 ""

data Env = MkEnv
  { authtoken :: BasicAuthData
  , clientEnv :: ClientEnv
  }

-- | Same as `mkEnvDef`, but you can change BaseUrl.
--   May be useful only if `host` is outdated.
mkEnv :: BaseUrl -> BasicAuthData -> HTTP.Manager -> Env
mkEnv host' authtoken httpManager = MkEnv {..}
  where
    clientEnv = mkClientEnv httpManager host'

-- | Default way to create client environment
mkEnvDef :: BasicAuthData -> HTTP.Manager -> Env
mkEnvDef = mkEnv host

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

trackAnonymusEvent :: Env -> TrackAnonymusEventBody -> IO (Either ClientError ())
trackAnonymusEvent MkEnv{..} body = do
  runClientM (trackAnonymusEventC authtoken body) clientEnv

reportPushMetricsC' :: Env -> ReportPushMetricsBody -> IO (Either ClientError ())
reportPushMetricsC' MkEnv{..} body = do
  runClientM (reportPushMetricsC body) clientEnv

trackCustomerEvent :: Env -> Text -> Text -> IO (Either ClientError ())
trackCustomerEvent env identifier = trackCustomerEventC' env identifier . defaultTrackCustomerEvent

reportPushMetrics :: Env -> IO (Either ClientError ())
reportPushMetrics env = reportPushMetricsC' env defaultReportPushMetrics
