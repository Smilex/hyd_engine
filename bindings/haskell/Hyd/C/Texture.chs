{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}

#include <texture.h>

{-# context lib="hyd" #-}

module Hyd.Texture where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Control.Monad
import Control.Applicative
import Data.Functor

data HydTexture = HydTexture
	{ ptr'hyd_tex :: Ptr ()
	, name'hyd_tex :: Ptr (CChar)
	}
instance Storable HydTexture where
	sizeOf _ = {#sizeof hyd_tex #}
	alignment _ = {#alignof hyd_tex #}
	peek p = HydTexture
			<$> ({#get hyd_tex->ptr #} p)
			<*> ({#get hyd_tex->name #} p)
	poke p x = do
		{#set hyd_tex.ptr #} p (ptr'hyd_tex x)
		{#set hyd_tex.name #} p (name'hyd_tex x)

{#pointer *hyd_tex as HydTexturePtr -> HydTexture #}

withT = with

{#fun hyd_tex_create_file as ^
	{`String', id `Ptr ()'} -> `HydTexture' withT* #}

{#fun hyd_tex_copy as ^
	{withT* `HydTexture'} -> `HydTexture' withT* #}

{#fun hyd_tex_destroy as ^
	{withT* `HydTexture'} -> `()' #}
