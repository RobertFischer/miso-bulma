{-# LANGUAGE NamedFieldPuns #-}

-- | This corresponds to the @.container@ class defined in "Layout/Container".
--   It is usually used as a direct child of a 'Miso.Bulma.Layout.Hero',
--   'Miso.Bulma.Layout.Section', 'Miso.Bulma.Layout.Footer', or
--   'Miso.Bulma.Components.Navbar'.

module Miso.Bulma.Layout.Container
	( Container
	, container
	, container'
	, container_
	, view
	, module Data.Default.Class
	) where

import Data.Default.Class ( Default (..) )
import Miso
import RFC.Prelude

-- | The type wrapping the container. Its particular implementation is subject to
--   change, so please use the relevant smart constructors, 'container', 'container'',
--   and 'container_'.
newtype Container action = Container ([Attribute action], [View action])
	deriving (Show,Eq,Ord,Typeable,Generic)

container :: View action -> Container action
-- ^ Provides a simple way to create a 'Container' with a single-element content and no attributes.
container view = Container (mempty, [view])
{-# INLINE container #-}

container' :: [View action] -> Container action
-- ^ Provides a simple way to create a 'Container' with multiple elements as content, but no attributes.
container' views = Container (mempty, views)
{-# INLINE container' #-}

container_ :: [Attribute action] -> [View action] -> Container action
-- ^ Provides a simple way to create a 'Container' with both attributes and multiple elements as content.
container_ attrs views = Container (attrs, views)
{-# INLINE container_ #-}

instance {-# OVERLAPS #-} Semigroup (Container action) where
	(<>) (Container (leftAttrs,leftViews)) (Container (rightAttrs,rightViews)) =
		Container (leftAttrs <> rightAttrs) (leftViews <> rightViews)
	{-# INLINE (<>) #-}

instance {-# OVERLAPS #-} Monoid (Container action) where
	mempty = Container (mempty,mempty)
	{-# INLINE mempty #-}

instance {-# OVERLAPS #-} Default (Container action) where
	def = mempty
	{-# INLINE def #-}

view :: Container action -> View action
-- ^ Views the container
view c                         | c == mempty = Html.text ""
view (Container (attrs,views)) = div_ ((class_ "container"):attrs) views
{-# INLINEABLE view #-}
