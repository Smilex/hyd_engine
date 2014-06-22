{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}

#include <sprite.h>

{-# context lib="hyd" #-}

module Hyd.Sprite where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Control.Monad
import Control.Applicative
import Data.Functor

import Hyd.Texture

data HydSprite = HydSprite
	{ tex'hyd_spr :: Ptr (HydTexture)
	, num_frames'hyd_spr :: Int
	, num_anims'hyd_spr :: Int
	}
instance Storable HydSprite where
	sizeOf _ = {#sizeof hyd_spr #}
	alignment _ = {#alignof hyd_spr #}
	peek p = HydSprite
			<$> ({#get hyd_spr->tex #} p)
			<*> ({#get hyd_spr->num_frames #} p)
			<*> ({#get hyd_spr->num_anims #} p)
	poke p x = do
		{#set hyd_spr.tex #} p (tex'hyd_spr x)
		{#set hyd_spr.num_frames #} p (num_frames'hyd_spr x)
		{#set hyd_spr.num_anims #} p (num_anims'hyd_spr x)

{#pointer *hyd_spr as HydSpritePtr -> HydSprite #}

withT = with

{#fun hyd_spr_create as ^
	{withT* `HydTexture'
	, id `Ptr ()'
	, `Int'
	, id `Ptr ()'
	, `Int'
	} -> `HydSprite' withT* #}

{#fun hyd_spr_create_file as ^
	{`String'
	, id `Ptr ()'
	, id `Ptr ()'
	} -> `HydSprite' withT* #}

{#fun hyd_spr_destroy as ^
	{withT* `HydSprite'} -> `()' #}
