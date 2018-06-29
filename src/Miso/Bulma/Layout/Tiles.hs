{-# LANGUAGE NamedFieldPuns #-}

module Miso.Bulma.Layout.Tiles
	( Tiles(..)
	, FramingTile(..)
	, ParentTile(..)
	, ChildTile(..)
	, TileDirection
	, verticalTileDirection
	, defaultTileDirection
	, tileDirectionAttr
	, TileSize
	, tileSizeAttr
	, is1
	, is2
	, is3
	, is4
	, is5
	, is6
	, is7
	, is8
	, is9
	, is10
	, is11
	, is12
	, isAuto
	, defaultTileSize
	, view
	, module Data.Default.Class
	) where

import Data.Default.Class ( Default (..) )
import Miso
import RFC.Prelude

-- | Direction of the tile, currently defined as either @is-vertical@ or not @is-vertical@.
newtype TileDirection = TileDirection Bool deriving (Read,Show,Eq,Typeable,Generic)

instance Ord TileDirection where
	compare (TileDirection True) (TileDirection True) = EQ
	compare (TileDirection True) (TileDirection False) = GT
	compare (TileDirection False) (TileDirection True) = LT
	compare (TileDirection False) (TileDirection False) = EQ
	{-# INLINE compare #-}

instance {-# OVERLAPPING #-} ToClassAttribute TileDirection where
	toClassName (TileDirection True)     =   "is-vertical"
	toClassName (TileDirection False)    = "is-horizontal"
	{-# INLINE toClassName #-}

-- | Defines the tile to be laid out with @is-vertical@
verticalTileDirection = TileDirection True
{-# INLINE verticalTileDirection #-}

instance Default TileDirection where
	def = TileDirection False
	{-# INLINE def #-}

{-|
	 Allows you to create multiple tiles by specifying multiple ancestors.
-}
data TileColumns view = TileColumns
	{ tileColumns  :: [An cestorTiles view] -- ^ The ancestors that define each column
	, columnsAttrs :: [Attribute view]  -- ^ The additional attributes to write onto the columns container
	, columnsSize  ::  Si ze -- ^ The size of the entire columns container
	} deriving (Show,Eq,Ord,Generic,Typeable)

instance Functor (TileColumns view) where
	fmap f (tc@TileColumns{tileColumns,columnAttrs}) = tc
		{ tileColumns = f <$$> tileColumns
		, columnsAttrs = f <$$> columnAttrs
		}
	{-# INLINEABLE fmap #-}

instance Semigroup (TileColumns view) where
	(<>) left right = TileColumns
		{ tileColumns = tileColumns left <> tileColumns right
		, columnsAttrs = columnsAttrs left <> columnsAttrs right
		, columnsSize = max (columnsSize left) (columnsSize right)
		}
	{-# INLINE (<>) #-}

instance Monoid (TileColumns view) where
	mempty = TileColumns [] [] def
	{-# INLINE mempty #-}

instance Default (TileColumns view) where
	def = mempty
	{-# INLINE def #-}

{-|
	The root data structure for a collection of tiles. This corresponds to the single root DOM instance
	holding @tile is-ancestor@.
-}
data AncestorTile view = AncestorTile
	{ framingTiles    :: [FramingTile view] -- ^ The framing elements for tiles.
	, framingAttr     ::  [Attribute view] -- ^ The additional attributes to write onto the ancestor.
	, framingSize     ::  TileSize -- ^ The size of the ancestor element.
	} deriving (Show,Eq,Ord,Generic,Typeable)

instance Functor (AncestorTile view) where
	fmap f (anc@AncestorTile{framingTiles,framingAttr}) = anc
		{ framingTiles = f <$$> tiles
		, framingAttr = f <$$> rootAttr
		}
	{-# INLINEABLE fmap #-}

instance Semigroup (AncestorTile view) where
	(<>) left right = Tiles
		{ framingTiles = framingTiles left <> framingTiles right
		, framingAttr = framingAttr left <> framingAttr right
		, framingSize = max (framingSize left) (framingSize right)
		}
	{-# INLINE (<>) #-}

instance Monoid (AncestorTile view) where
	mempty = AncestorTile def def def
	{-# INLINE mempty #-}

instance Default (AncestorTile view) where
	def = mempty
	{-# INLINE def #-}

-- | Represents a frame around tiles, which may itself contain other frames or
--   it may only hold parents. If 'startFrame', 'endFrame', and 'frameAttr' are
--   all 'null', and there is only one element in 'framed', then the framing
--   tile will be omitted in rendering.
data FramingTile view = FramingTile
	{ startFrame        :: [FramingTile view] -- ^ Written out before @framed@
	, endFrame          :: [FramingTile view] -- ^ Written out after @framed@
	, framed            :: [ParentTile view]  -- ^ Written out between @startFrame@ and @endFrame@
	, frameAttr         :: [Attribute view]   -- ^ Attributes attached to the frame
	} deriving (Show,Eq,Ord,Generic,Typeable)

instance Functor (FramingTile view) where
	fmap f (FramingTile{startFrame,endFrame,framed,frameAttr}) = FramingTile
		{ startFrame = f <$$> startFrame
		, endFrame = f <$$> endFrame
		, framed = f <$$> framed
		, frameAttr = f <$$> frameAttr
		}
	{-# INLINEABLE fmap #-}

instance Semigroup (FramingTile view) where
	(<>) left right = FramingTile
		{ startFrame = startFrame left <> startFrame right
		, endFrame = endFrame left <> endFrame right
		, framed = framed left <> framed right
		, frameAttr = frameAttr left <> frameAttr right
		}
	{-# INLINE (<>) #-}

instance Monoid (FramingTile view) where
	mempty = FramingTile def def def def
	{-# INLINE mempty #-}

instance Default (FramingTile view) where
	def = mempty
	{-# INLINE def #-}

-- | Represents a parent tile, which contains one more more children.
data ParentTile view = ParentTile
	{ childTiles     :: [ChildTile view] -- ^ The child tiles to display in this parent
	, parentAttr     :: [Attribute view] -- ^ Additional parent attributes
	, parentSize     :: TileSize -- ^ The size of this parent
	, parentDir      :: TileDirection -- ^ Direction of this tile's layout
	} deriving (Show,Eq,Ord,Generic,Typeable)

instance Functor (ParentTile view) where
	fmap f (parent@ParentTile{childTiles,parentAttr}) = parent
		{ childTiles = f <$$> childTiles
		, parentAttr = f <$$> parentAttr
		}
	{-# INLINEABLE fmap #-}

instance Semigroup (ParentTile view) where
	(<>) left right = ParentTile
		{ childTiles = childTiles left <> childTiles right
		, parentSize = max (parentSize left) (parentSize right)
		, parentDir = max (parentDir left) (parentDir right)
		, parentAttr = parentAttr left <> parentAttr right
		}
	{-# INLINE (<>) #-}

instance Monoid (ParentTile view) where
	mempty = ParentTile [] [] defaultTileSize defaultTileDirection
	{-# INLINE mempty #-}

instance Default (ParentTile view) where
	def = mempty
	{-# INLINE def #-}

-- | Represents a child tile, which actually has content.

