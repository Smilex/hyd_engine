{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}

#include <scene.h>

{-# context lib="hyd" #-}

module Hyd.Scene where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Control.Monad
import Control.Applicative
import Data.Functor

data HydScene = HydScene
	{ {-#entities'hyd_scene :: Ptr ()
	, sprites'hyd_scene :: Ptr ()#-}
	}
instance Storable HydScene where
	sizeOf _ = {#sizeof hyd_scene #}
	alignment _ = {#alignof hyd_scene #}
	peek p = HydScene
			{-#<$> ({#get hyd_scene->entities #} p)
			<*> ({#get hyd_scene->sprites #} p)#-}
	poke p x = do
		{-#{#set hyd_scene.entities #} p (entities'hyd_scene x)
		{#set hyd_scene.sprites #} p (sprites'hyd_scene x)#-}

{#pointer *hyd_scene as HydScenePtr -> HydScene #}

withT = with

{#fun hyd_scene_create as ^
	{} -> `HydScene' withT* #}

{#fun hyd_scene_create_file as ^
	{ `String'
	, id `Ptr ()'
	, id `Ptr ()'
	} -> `HydScene' withT* #}

{#fun hyd_scene_destroy as ^
	{withT* `HydScene'} -> `()' #}

{#fun hyd_scene_draw as ^
	{withT* `HydScene', id `Ptr ()'} -> `()' #}
