module Miso.Bulma.Versions
	( module Miso.Bulma.Versions ) where

import Miso
import Network.URI
import RFC.Prelude

-- | The version of Bulma that we built this library against
bulmaVersion :: StrictText
bulmaVersion = "0.7.1"
{-# INLINE bulmaVersion #-}

-- | The CDN URL to get the minified CSS of the Bulma version that we built this library against.
bulmaCdnCss :: URI
bulmaCdnCss =
	fromMaybe
		(error "Could not parse Bulma CDN CSS URI")
		(parseURI . fromText $ "https://cdnjs.cloudflare.com/ajax/libs/bulma/" <> bulmaVersion <> "/css/bulma.min.css")
{-# INLINEABLE bulmaCdnCss #-}

-- | The CDN URL to get the sourcemap of the minified CSS of the Bulma version that we built this library against.
bulmaCdnCssMap :: URI
bulmaCdnCssMap =
	fromMaybe
		(error "Could not parse Bulma CDN CSS sourcemap URI")
		(parseURI . fromText $ "https://cdnjs.cloudflare.com/ajax/libs/bulma/" <> bulmaVersion <> "/css/bulma.css.map")
{-# INLINEABLE bulmaCdnCssMap #-}

-- | The version of Wikiki's Bulma-Extension library that we built this library against
bulmaExtensionsVersion :: StrictText
bulmaExtensionsVersion = "2.2.1"
{-# INLINE bulmaExtensionsVersion #-}

-- | The CDN URL to get the minified JavaScript of `bulma-extensions`.
--   You're on your own for Bulma Extensions CDN CSS. If you know of one, submit a PR!
bulmaExtensionsCdnJs :: URI
bulmaExtensionsCdnJs =
	fromMaybe
		(error "Could not parse the Bulma Extension CDN JS URI")
		(parseURI . fromText $
			"https://cdn.jsdelivr.net/npm/bulma-extensions@" <> bulmaExtensionsVersion <> "/dist/js/bulma-extensions.min.js"
		)
{-# INLINEABLE bulmaExtensionsCdnJs #-}

