{-# LANGUAGE NamedFieldPuns #-}

-- | This corresponds to the @.level@ class (and its children)
--   defined in "Layout/Level".

module Miso.Bulma.Layout.Level
	( Level(..)
	, LevelSide(..)
	, Level'(..)
	, LevelItem(..)
	, LevelPosition
	, level
	, level'
	, level_
	, centeredLevel
	, centeredLevel'
	, centeredLevel_
	, levelItem
	, levelItem'
	, levelItem_
	, view
	, view'
	, module Data.Default.Class
	) where

import Data.Default.Class ( Default (..) )
import Miso
import Miso.Classy.Views
import RFC.Prelude

-- | The position of the 'LevelItem' within the 'Level': 'levelLeft' or 'levelRight'?
--   The implementation of this type is subject to change and should not be relied upon.
newtype LevelPosition = LevelPosition Bool deriving (Read,Show,Eq,Ord,Enum,Bounded,Typeable,Generic)

instance Ord LevelPosition where
	compare left right
		| left == right = EQ
		| left == levelLeft = LT
		| left == levelRight = GT
	{-# INLINE compare #-}

-- | The 'LevelPosition' denoting that the corresponding items go on the left side of the level.
levelLeft :: LevelPosition
levelLeft = LevelPosition True
{-# INLINE levelLeft #-}

-- | The 'LevelPosition' denoting that the corresponding items go on the right side of the level.
levelRight = LevelPosition (not leftVal)
	where
		(LevelPosition leftVal) = levelLeft
{-# INLINE levelRight #-}

instance {-# OVERLAPS #-} Default (LevelPosition) where
	def = levelLeft
	{-# INLINE def #-}

-- | Type representing items within the level.
newtype LevelItem action = LevelItem ([Attributes action], [View action]) deriving (Show,Eq,Ord,Typeable,Generic,Semigroup,Monoid,Default)

-- | Type representing the content and attributes of a side of a level (left and right).
newtype LevelSide action = LevelSide ([Attributes action], [LevelItem action]) deriving (Show,Eq,Ord,Typeable,Generic,Semigroup,Monoid,Default)

-- | Type representing a @level@ with a left an a right side.
data Level action = Level
	{ isMobile :: Bool                   -- ^ Does the level display horizontal on mobile devices?
	, attrs :: [Attributes action]       -- ^ Attributes attached to the level itself.
	, leftSide :: LevelSide action       -- ^ Left side of the level
	, rightSide :: LevelSide action      -- ^ Right side of the level
	} deriving (Show,Eq,Ord,Typeable,Generic)

-- | Type representing a @level@ where everything is centered and there are no sides.
data Level' action = Level'
	{ isMobile' :: Bool
	, attrs' :: [Attributes action]
	, items' :: [LevelItem action]
	} deriving (Show,Eq,Ord,Typeable,Generic)

-- | Creates a single-item centered level.
centeredLevel  :: LevelItem action -> Level' action
centeredLevel = centeredLevel' . (:[])
{-# INLINE centeredLevel #-}

-- | Creates a centered level with multiple items.
centeredLevel' :: [LevelItem action] -> Level' action
centeredLevel' = centeredLevel_ []
{-# INLINE centeredLevel' #-}

-- | Creates a centered level with multiple items and some attributes on the container.
centeredLevel_ :: [Attribute action] -> [LevelItem action] -> Level' action
centeredLevel_ attrs items = Level { isMobile' = False, attrs' = attrs, items' = items }
{-# INLINE centeredLevel_ #-}

level :: LevelItem action -> LevelItem action -> Level action
-- ^ Simple level: takes in a single item for the left and another for the right and returns the level which will render those.
level leftItem rightItem = level' [leftItem] [rightItem]
{-# INLINE level #-}

level' :: [LevelItem action] -> [LevelItem action] -> Level action
-- ^ Slightly more advanced level compared to 'level'. Here, we allow multiple items on the left and the right.
level' leftItems rightItems = level_ mempty (mempty, leftItems) (mempty, rightItems)
{-# INLINE level' #-}

level_ :: [Attribute action] -> ([Attribute action], [LevelItem action]) -> ([Attribute action], [LevelItem action]) -> Level action
-- ^ Most advanced form of 'level', allowing both multiple items on the left and the right and the ability to add attributes to the left
--   and right.
level_ attrs leftPair rightPair = Level (False, attrs, (LevelSide leftPair), (LevelSide rightPair))
{-# INLINE level_ #-}

levelItem :: (ToView' v action) => v action -> LevelItem action
-- ^ Simple constructor for a level item, which only has one view within it.
levelItem viewable = levelItem' [viewable]
{-# INLINE levelItem #-}

levelItem' :: (ToView' v action) => [v action] -> LevelItem action
-- ^ Slightly more advanced constructor for a level item compared to 'levelItem'. Here, we allow multiple items.
levelItem' viewables = levelItem_ mempty viewables
{-# INLINE levelItem' #-}

levelItem_ :: (ToView' v action) => [Attribute action] -> [v action] -> LevelItem action
-- ^ Most advanced form of 'levelItem', allowing attributes and multiple items to be specified for the item.
levelItem_ attrs viewables = LevelItem (attrs, toView <$> viewables)
{-# INLINE levelItem_ #-}

view' :: Level' action -> View action
-- ^ Views a 'Level''.
view' Level{attrs',isMobile',items'} =
	div_ ((classList_ [("level",True),("is-mobile",isMobile')]):attrs') $
		viewItem <$> items'
{-# INLINEABLE view' #-}

view :: Level action -> View action
-- ^ Views a 'Level'
view (Level{attrs,isMobile,leftSide,rightSide}) =
	div_ ((classList_ [("level",True),("is-mobile",isMobile)]):attrs)
		[ viewSide levelLeft leftSide
		, viewSide levelRight rightSide
		]
{-# INLINEABLE view #-}

viewSide :: LevelPosition -> LevelSide action -> View action
viewSide _ side | side == mempty = Html.text ""
viewSide pos (ViewSide (attrs, items)) =
	div_ ((classList_ [("level-left", pos == levelLeft), ("level-right", pos == levelRight)]):attrs) $ viewItem <$> items
{-# INLINEABLE viewSide #-}

viewItem :: LevelItem action -> View action
viewItem (LevelItem (attrs,views)) = div_ ((class_ "level-item"):attrs) views
{-# INLINE viewItem #-}
