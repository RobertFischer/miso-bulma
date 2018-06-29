{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase   #-}

{- |
	Miso-ification of the "typography helper" modifiers: https://bulma.io/documentation/modifiers/typography-helpers/
-}
module Miso.Bulma.Modifiers.Typography
	( TextSize(..)
	, ResponsiveSize(..)
	, Viewport(..)
	, Colored(..)
	, mkColored
	, ColorScope(..)
	, Color(..)
	, Alignment(..)
	) where

import Data.Default
import Miso
import Miso.Bulma.Internal
import Miso.Bulma.Modifiers.Color      ( Color (..), ColorScope (..), Colored (..), mkColored )
import Miso.Bulma.Modifiers.Responsive ( Viewport (..), viewportSuffix )
import Miso.Classy.Attributes
import RFC.Prelude

-- | The typographical size, which goes from 1 to 7 and generates the classes
--   @is-size-1@, @is-size-2@, ..., @is-size-7@.
data IsSize
	= IsDefaultSize
	| IsSize Word8
	| ResponsiveSize Viewport IsSize
	deriving (Eq,Ord,Show,Generic,Typeable)

instance Bounded IsSize where
	minBound = 0
	{-# INLINE minBound #-}

	maxBound = 7
	{-# INLINE maxBound #-}

instance Default IsSize where
	def = IsDefaultSize
	{-# INLINE def #-}

instance IsClassAttribute IsSize where
	toClassName IsDefaultSize        = "is-default-size"
	toClassName (IsSize n)           = toMisoString $ "is-size-" <> show n
	{-# INLINE toClassName #-}

-- | Smart constructor for 'IsSize'. The argument is clamped to between 'minBound' and
--   'maxBound' for 'IsSize'.
mkIsSize :: (Integral i, Ord i) => i -> IsSize
mkIsSize i
	| i < minBound = IsSize minBound
	| i > maxBound = IsSize maxBound
	| _ = IsSize . fromInteger $ toInteger i
{-# INLINE mkIsSize #-}

-- | Smart constructor for 'IsSize' which provides a sizing applicable only to certain screen widths.
mkIsSizeResponsive :: (Integral i, Ord i) => Viewport -> i -> IsSize
mkIsSizeResponsive v = Responsive v . mkIsSize
{-# INLINE mkIsSizeResponsive #-}

-- | Text alignment
data Alignment
	= Justified
	| Centered
	| Left
	| Right
	| DefaultAlignment
	deriving (Eq,Ord,Enum,Bounded,Show,Generic,Typeable)

instance IsClassAttribute Alignment where
	prefix = "has-text-"
	{-# INLINE prefix #-}

-- | Text transformation
data Transform
	= IsUppercase
	| IsCapitalized
	| IsLowercase
	| IsItalic
	deriving (Eq,Ord,Enum,Bounded,Show,Generic,Typeable)

instance IsClassAttribute Transform

-- | Text weight
data Weight
	= Light
	| Normal
	| Semibold
	| Bold
	deriving (Eq,Ord,Enum,Bounded,Show,Generic,Typeable)

instance IsClassAttribute Weight where
	prefix _ = "has-text-weight-"
	{-# INLINE prefix #-}


