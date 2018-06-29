{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
	Implementation of the "responsive helper" modifiers: https://bulma.io/documentation/modifiers/responsive-helpers/
-}
module Miso.Bulma.Modifiers.Responsive
	( Display(..)
	, Viewport(..)
	, viewportSuffix
	, Responsive(..)
	) where

import Data.Default
import Miso
import Miso.Bulma.Internal
import Miso.Classy.Attributes
import RFC.Prelude
import Text.Inflections       ( toDashed )

-- | A wrapper around a 'ToClassAttribute' or 'ToView'' that constrians the
--   element to acting only when a specific 'Viewport' matches the current
--   width of the screen.
newtype Responsive elt = Responsive (Viewport, elt)
instance Eq elt => Eq (Responsive elt)
instance Show elt => Show (Responsive elt)
instance Ord elt => Ord (Responsive elt)
instance Enum elt => Enum (Responsive elt)
instance Bounded elt => Bounded (Responsive elt)
instance Typeable elt => Typeable (Responsive elt)
instance Generic elt => Generic (Responsive elt)
instance Default elt => Default (Responsive elt)

-- | Wraps the @elt@ in a @div@ which is hidden except when the 'Viewport'
--   of the 'Responsive' matches the current width of the screen. When
--   the viewport matches, it is displayed as block ('IsBlock').
instance ToView' elt action => ToView' (Responsive elt) action where
	toView' (Responsive (viewport, elt)) =
		div_
			[ toAttribute IsHidden, toAttribute $ Responsive (viewport, IsBlock) ]
			[ toView' elt ]

-- | Appends the 'viewportSuffix' for the viewport after the class name of the attribute.
instance ToClassAttribute attr => ToClassAttribute (Responsive attr) where
	toClassName (Responsive (viewport, attr)) = toClassName attr <> viewportSuffix viewport
	{-# INLINEABLE toClassName #-}

-- | The display classes, of which we include @hidden@.
data Display
	= IsBlock
	| IsFlex
	| IsInline
	| IsInlineBlock
	| IsInlineFlex
	| IsHidden
	deriving (Eq,Ord,Show,Enum,Bounded,Generic,Typeable,IsClassAttribute)

instance Default Display where
	def = IsBlock
	{-# INLINE def #-}

-- | Viewport specifiers for the display modifiers.
data Viewport
	= AllSizes
	| Mobile
	| TabletOnly
	| Touch
	| Tablet
	| Desktop
	| DesktopOnly
	| Widescreen
	| WidescreenOnly
	| FullHD
	deriving (Eq,Ord,Show,Enum,Bounded,Generic,Typeable)

instance Default Viewport where
	def = AllSizes
	{-# INLINE def #-}

-- | Calculates the suffix for a given viewport
viewportSuffix :: Viewport -> MisoString
viewportSuffix AllSizes = ""
viewportSuffix FullHD   = "-fullhd"
viewportSuffix other    = "-" <> dasherizeMS other
{-# INLINEABLE viewportSuffix #-}

