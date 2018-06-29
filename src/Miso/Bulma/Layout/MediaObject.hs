module Miso.Bulma.Layout.MediaObject
	( MediaObject(..)
	, view
	) where

import Miso
import RFC.Prelude

-- | The state of a media object.
data MediaObject action = MediaObject
	{ mediaLeft :: [View action]
	, mediaContent :: [View action]
	, mediaRight :: [View action]
	} deriving (Eq,Ord,Show,Generic,Typeable)
{-# INLINE mediaLeft #-}
{-# INLINE mediaContent #-}
{-# INLINE mediaRight #-}

instance Functor (MediaObject action) where
	fmap f MediaObject{mediaLeft,mediaContent,mediaRight} =
		MediaObject
			{ mediaLeft = f <$$> mediaLeft
			, mediaContent = f <$$> mediaContent
			, mediaRight = f <$$> mediaRight
			}
	{-# INLINEABLE fmap #-}

view :: MediaObject action -> View action
view MediaObject{mediaLeft,mediaContent,mediaRight} =
	article_ [ className "media" ]
		[ div_ [ className "media-left" ] mediaLeft
		, div_ [ className "media-content" ] mediaContent
		, div_ [ className "media-right" ] mediaRight
		]
{-# INLINEABLE view #-}
