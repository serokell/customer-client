-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: Apache-2.0

module Customer.Aeson (defaultAesonOptions) where

import Data.Aeson (Options)
import Data.Aeson.Casing ( aesonPrefix, snakeCase )

defaultAesonOptions :: Options
defaultAesonOptions = aesonPrefix snakeCase
