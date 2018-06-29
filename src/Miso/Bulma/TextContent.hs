{-# LANGUAGE MultiParamTypeClasses #-}

-- | This is the module correlating to the mention of @.content@ in
--   "Overview > CSS Classes" in the documentation. It specifically
--   provides support for responsive and pleasant rendering of textual
--   components.

module Miso.Bulma.TextContent
	( viewText
	, viewText'
	, viewText_
	, textContentClass
	) where

import Miso
import RFC.Prelude
import RFC.String

textContentClass :: MisoString
-- ^ The class that denotes we are in text content.
textContentClass = "content"
{-# INLINE textContentClass #-}

viewText :: MisoString -> View action
-- ^ Provides a simple view of some text without any modifying attributes.
viewText = viewText' []
{-# INLINE viewText #-}

viewText' :: [Attributes action] -> MisoString -> View action
-- ^ Provides a view of some text with the enclosing 'Html.div_' having the provided attributes.
viewText' attrs str = viewText_ attrs [text str]
{-# INLINE viewText' #-}

viewText_ :: [Attributes action] -> [View action] -> View action
-- ^ Wraps view list with an 'Html.span_' with the 'textContentClass' assigned,
--   and then wraps that result into an 'Html.div_' with the given attributes.
viewText_ attrs content = div_ attrs $ span_ [ class_ textContentClass ] content
{-# INLINEABLE viewText_ #-}
