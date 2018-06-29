{-# LANGUAGE MultiParamTypeClasses #-}

module Miso.Bulma.Columns
	( Columns(..)
	, ColumnGap
	, columnGap
	, Column(..)
	, Size(..)
	) where

import Miso
import Miso.Bulma.Internal
import Miso.Bulma.Modifiers.Responsive
import Miso.Bulma.Size
import Miso.Classy.Views
import RFC.Prelude

newtype ColumnGap = ColumnGap Word8
	deriving (Eq,Show,Ord,Generic,Typeable)

instance Bounded ColumnGap where
	minBound = ColumnGap 0
	{-# INLINE minBound #-}

	maxBound = ColumnGap 8
	{-# INLINE maxBound #-}

instance Default ColumnGap where
	def = ColumnGap 3
	{-# INLINE def #-}

-- | Represents the container that wraps multiple columns.
data Columns action = Columns
	{ columns :: [Column action]
	, columnsAttrs :: [Attribute action]
	, columnsGaps :: Size
	, columnsMultiline :: Bool
	} deriving (Eq,Ord,Show,Generic,Typeable)

instance Default (Columns action) where
	def = Columns [] [] def False
	{-# INLINE def #-}

colsToAttrs :: Columns action -> [Attribute action]
colsToAttrs Columns{columnsGaps,columnsMultiline,columnsAttrs} =
		gaps $ multi $ (class_ "columns"):columnsAttrs
	where
		gapToN (ColumnGap n) = n
		gaps rest =
			if columnsGaps == def then
				rest
			else
				(class_ "is-variable"):(class_ $ "is-" <> show n):rest
		multi rest =
			if columnsMultiline then
				(class_ "is-multiline"):rest
			else
				rest
{-# INLINEABLE colsToAttrs #-}

instance {-# OVERLAPS #-} ToView' (Responsive (Columns action)) action where
	toView' (Responsive (viewport, cols@Columns{columns})) =
		div_ (
			(class_ $ "is-" <> viewportSuffix viewport):colsToAttrs cols
		) $ toView' <$> columns
	{-# INLINEABLE toView' #-}

instance {-# OVERLAPPABLE #-} ToView' (Columns action) action where
	toView' (cols@Columns{columns}) =
		div_ (
			(class_ "columns"):colsToAttrs cols
		) $ toView' <$> columns
	{-# INLINEABLE toView' #-}

instance Functor (Columns action) where
	fmap f Columns{columns,columnsAttrs} = Columns
		{ columns = f <$> columns
		, columnAttrs = f <$> columnAttrs
		}
	{-# INLINE fmap #-}

instance Semigroup (Columns action) where
	(<>) a b = Columns
		{ columns = columns a <> columns b
		, columnAttrs = columnAttrs a <> columnAttrs b
		, columnsGapless = columnsGapless a || columnsGapless b
		, columnsMultiline = columnsMultiline a || columnsMultiline b
		}
	{-# INLINEABLE (<>) #-}

-- | Represents a single column within 'Columns'.
data Column action = Columns
	{ columnContent :: [View action]
	, columnAttrs :: [Attribute action]
	, columnSize :: Size
	} deriving (Eq,Ord,Show,Generic,Typeable)

instance Default (Column action) where
	def = Columns def def def
	{-# INLINE def #-}

instance ToView' (Column action) where
	toView' Column{columnContent,columnAttrs,columnSize} =
		div _ ((class_ "column"):(toAttribute columnSize):columnAttrs) columnContent
	{-# INLINEABLE toView' #-}

instance Functor (Column action) where
	fmap f c@Column{columnContent,columnAttrs} = c
		{ columnContent = f <$> columnContent
		, columnAttrs = f <$> columnAttrs
		, columnSize = columnSize
		}
	{-# INLINE fmap #-}

instance Semigroup (Column action) where
	(<>) a b = Column
		{ columnContent = columnContent a <> columnContent b
		, columnAttrs = columnAttrs a <> columnAttrs b
		, columnSize = max (columnSize a) (columnSize b)
		}
	{-# INLINEABLE (<>) #-}


