{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}

#include <input.h>

{-# context lib="hyd" #-}

module Hyd.Input where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Control.Monad
import Control.Applicative
import Data.Functor

import Hyd.Texture

data HydInputPreset = HydInputPreset
	{ name'hyd_input_preset :: String
	}
instance Storable HydInputPreset where
	sizeOf _ = {#sizeof hyd_input_preset #}
	alignment _ = {#alignof hyd_input_preset #}
	peek p = HydInputPreset
			<$> ({#get hyd_input_preset->name #} p)
	poke p x = do
		{#set hyd_input_preset.name #} p (name'hyd_input_preset x)

{#pointer *hyd_input_preset as HydInputPresetPtr -> HydInputPreset #}

withT = with

{#fun hyd_input_preset_get_action_value as ^
	{withT* `HydInputPreset'
	, `String'
	} -> `Int' #}

{#fun hyd_input_get_max_value as ^
	{} -> `Int' #}
