{- |
		This module is for internal use only. Don't even think about depending on it
		outside of this library.
-}
module Miso.Bulma.Internal
	( module Miso.Bulma.Internal
	) where

import Miso.String
import RFC.Prelude
import Text.Inflections ( toDashed )

-- | Provides
dasherizeMS :: (Show s) => s -> MisoString
dasherizeMS str = toMisoString . fromRight txt $ dasherize txt
	where
		txt = toText $ show str
{-# INLINEABLE dasherizeMS #-}
