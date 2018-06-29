{-# LANGUAGE MultiParamTypeClasses #-}

-- | This is the module correlating to "Columns > Sizes" in the documentation.
--   Since the column sizing is used throughout the rest of the platform, it
--   gets its own module so that it can be imported without also having to
--   import all the entirety of 'Miso.Bulma.Columns'.

module Miso.Bulma.Size
	( Size(..)
	, TwelveCol
	, sizeToRational
	, isSize
	, sizeSuffix
	) where

import Miso
import Miso.Classy.Attributes
import RFC.Prelude

-- | Represents somewhere between 1 and 11 columns of width.
data TwelveCol = ColCount Word8
	deriving (Eq,Ord,Show,Generic,Typeable)

instance Bounded TwelveCol where
	minBound = ColCount 1
	{-# INLINE minBound #-}

	maxBound = ColCount 11
	{-# INLINE maxBound #-}

-- | Represents the various different ways that you can specify a size.
data Size
	= Default
	| FourFifths
	| ThreeQuarters
	| TwoThirds
	| ThreeFifths
	| Half
	| TwoFifths
	| OneThird
	| OneQuarter
	| OneFifth
	| Narrow
	| Is TwelveCol -- ^ Use 'isSize' instead of this constructor.
	deriving (Eq,Show,Bounded,Enum,Generic,Typeable)

-- | Smart constructor for the 'Is' constructor of 'Size'. Clamps the
--   values within the bounds of 'TwelveCol'.
isSize :: Word8 -> Size
isSize = Is . max minBound . min maxBound . ColCount
{-# INLINEABLE isSize #-}

-- | Converts the size to its corresponding rational width, with 'Default' and 'Narrow' being the same as 'Is 1'.
sizeToRational :: Size -> Rational
sizeToRational Default           = 1 % 12
sizeToRational FourFifths        = 4 % 5
sizeToRational ThreeQuarters     = 3 % 4
sizeToRational TwoThirds         = 2 % 3
sizeToRational ThreeFifths       = 3 % 5
sizeToRational Half              = 1 % 2
sizeToRational TwoFifths         = 2 % 5
sizeToRational OneThird          = 1 % 3
sizeToRational OneQuarter        = 1 % 4
sizeToRational OneFifth          = 1 % 5
sizeToRational Narrow            = 1 % 12
sizeToRational (Is (ColCount n)) = (toInteger n) % 12
{-# INLINEABLE sizeToRational #-}

instance Ord Size where
	compare Default Default        = EQ
	compare Narrow Narrow          = EQ
	compare Narrow _               = LT
	compare _ Narrow               = GT
	compare Default b              = compare (Is (ColCount 1)) b
	compare a Default              = compare a (Is (ColCount 1))
	compare a b                    = compare (sizeToRational a) (sizeToRational b)
	{-# INLINEABLE compare #-}

instance Default Size where
	def = Default
	{-# INLINE def #-}

instance ToClassAttribute Size where
	toClassName Default                  = "is-default-size"
	toClassName (Is (ColCount n))        = toMisoString $ "is-" <> show n
	toClassName other                    = "is-" <> sizeSuffix other
	{-# INLINEABLE toClassName #-}

-- | Provides the suffix for the given size
sizeSuffix :: Size -> MisoString
sizeSuffix Default           = ""
sizeSuffix (Is (ColCount n)) = toMisoString $ '-':show n
sizeSuffix other             = "-" <> dasherizeMS other
{-# INLINEABLE sizeSuffix #-}

-- | Represents an offset within the column grid
newtype Offset = Offset Size
	deriving (Eq,Ord,Show,Bounded,Enum,Generic,Typeable,Default)

instance ToClassAttribute Offset where
	toClassName (Offset size) = "offset" <> sizeSuffix size
	{-# INLINE toClassName #-}


