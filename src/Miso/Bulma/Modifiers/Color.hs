{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase   #-}

module Miso.Bulma.Modifiers.Color
	( Color(..)
	, ColorScope(..)
	, Colored(..)
	, mkColored
	) where

import Data.Default
import Miso
import Miso.Bulma.Internal
import Miso.Classy.Attributes
import RFC.Prelude
import Text.Inflections       ( toDashed )

-- | Whether you are specifing the text or background color.
data ColorScope
	= HasText
	| HasBackground
	deriving (Eq,Ord,Show,Enum,Bounded,Generic,Typeable)

-- | Which particular color your are specifying
data Color
	= Light
	| Dark
	| Primary
	| Info
	| Link
	| Success
	| Warning
	| Danger
	| Black
	| BlackBis
	| BlackTer
	| GreyDarker
	| GreyDark
	| Grey
	| GreyLight
	| GreyLighter
	| WhiteTer
	| WhiteBis
	| White
	deriving (Eq,Ord,Show,Enum,Bounded,Generic,Typeable)

-- | A specification of a display for a certain viewport selector.
newtype Colored = Colored (ColorScope, Color)
	deriving (Eq,Ord,Show,Enum,Bounded,Generic,Typeable)

mkColored :: ColorScope -> Color -> Colored
mkColored scope color = Colored (scope,color)

instance IsClassAttribute Colored where
	toClassName (Colored (scope,color)) = dasherizeMS scope <> "-" <> dasherizeMS color


