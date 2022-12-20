-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: Apache-2.0

module Customer.Aeson (defaultAesonOptions) where

import Data.Aeson (Options(fieldLabelModifier), camelTo2, defaultOptions)

defaultAesonOptions :: String -> Options
defaultAesonOptions prefix = defaultOptions
  { fieldLabelModifier = camelToSnake . dropPrefix
  }
  where
    camelToSnake = camelTo2 '_'
    dropPrefix = drop (length prefix)
