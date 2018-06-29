{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase   #-}

module Miso.Bulma.Modifiers.Syntax
	( Color(..)
	, Size(..)
	, State(..)
	) where

import Data.Default
import Misa.Bulma.Classes
import Miso
import RFC.Prelude

-- | Provides the basic spec for colors.
data Color
		= IsPrimary
		| IsLink
		| IsInfo
		| IsSuccess
		| IsWarning
		| IsDanger
		| IsDefaultColor -- ^ @is-default-color@ (not defined in Bulma)
	deriving (Eq,Ord,Show,Enum,Bounded,Generic,Typeable)
instance IsClassAttribute Color
instance Default Color where
	def = IsDefaultColor

-- | Provides the basic spec for relative size. These values have no corolation
--   to the 12-column attributes (eg: @is-2@, @is-6@, etc.), nor to the typography
--   sizing (eg: @is-size-3@). They purely specify size relative to the others.
data Size
	= IsSmall
	| IsMedium
	| IsLarge
	| IsDefaultSize -- ^ @is-default-size@ (not defined in Bulma)
	deriving (Eq,Ord,Show,Enum,Bounded,Generic,Typeable)
instance IsAttribute Size
instance Default Size where
	def = IsDefaultSize
	{-# INLINE def #-}

-- | The state or style of the element.
data State
	= IsOutlined
	| IsLoading
	| IsDisabled
	| IsDefaultState -- ^ @is-default-state@ (not defined in Bulma)
	deriving (Eq,Show,Enum,Bounded,Generic,Typeable)
instance IsAttribute' State where
	toAttribute' = \case
		IsDisabled -> disabled_ True
		other -> class_ $ dasherizeMS other
	{-# INLINE toAttribute' #-}

instance Default State where
	def = IsDefaultState
	{-# INLINE def #-}
