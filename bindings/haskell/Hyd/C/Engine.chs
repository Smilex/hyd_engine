{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}

#include <engine.h>

{-# context lib="hyd" #-}

module Hyd.Engine where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Control.Monad
import Control.Applicative
import Data.Functor

import Hyd.Scene
import Hyd.Input

data HydEngine = HydEngine
	{ running'hyd_engine :: Bool
	, renderer'hyd_engine :: Ptr ()
	, current_scene'hyd_engine :: Ptr (HydScene)
	, current_input_preset'hyd_engine :: Ptr (HydInputPreset)
	}
instance Storable HydEngine where
	sizeOf _ = {#sizeof hyd_engine #}
	alignment _ = {#alignof hyd_engine #}
	peek p = HydEntity
			<$> ({#get hyd_engine->running #} p)
			<*> ({#get hyd_engine->renderer #} p)
			<*> ({#get hyd_engine->current_scene #} p)
			<*> ({#get hyd_engine->current_input_preset #} p)
	poke p x = do
		{#set hyd_engine.running #} p (running'hyd_engine x)
		{#set hyd_engine.renderer #} p (renderer'hyd_engine x)
		{#set hyd_engine.current_scene #} p (current_scene'hyd_engine x)
		{#set hyd_engine.current_input_preset #} p (current_input_preset'hyd_engine x)

{#pointer *hyd_ent as HydEntityPtr -> HydEntity #}

withT = with

{#fun hyd_engine_create as ^
	{} -> `HydEngine' withT* #}

{#fun hyd_engine_init as ^
	{ withT* `HydEngine'
	, id `Ptr (String)'
	} -> `Bool' #}

{#fun hyd_engine_destroy as ^
	{ withT* `HydEngine'
	} -> `()' #}

{#fun hyd_engine_run as ^
	{ withT* `HydEngine'
	} -> `Bool' #}

{#fun hyd_engine_load_scene as ^
	{ withT* `HydEngine'
	, `String'
	} -> `Bool' #}

{#fun hyd_engine_load_input_preset as ^
	{ withT* `HydEngine'
	, `String'
	} -> `Bool' #}
