{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}

#include <entity.h>

{-# context lib="hyd" #-}

module Hyd.Entity where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Control.Monad
import Control.Applicative
import Data.Functor

import Hyd.Sprite

data HydEntity = HydEntity
	{ spr'hyd_ent :: Ptr (HydSprite)
	, name'hyd_ent :: String
	, parent'hyd_ent :: Ptr (HydEntity)
	}
instance Storable HydEntity where
	sizeOf _ = {#sizeof hyd_ent #}
	alignment _ = {#alignof hyd_ent #}
	peek p = HydEntity
			<$> ({#get hyd_ent->spr #} p)
			<*> ({#get hyd_ent->name #} p)
			<*> ({#get hyd_ent->parent #} p)
	poke p x = do
		{#set hyd_ent.spr #} p (spr'hyd_ent x)
		{#set hyd_ent.name #} p (name'hyd_ent x)
		{#set hyd_ent.parent #} p (parent'hyd_ent x)

{#pointer *hyd_ent as HydEntityPtr -> HydEntity #}

withT = with

{#fun hyd_ent_create as ^
	{ withT* `HydSprite'
	, `String'
	, withT* `HydEntity'
	} -> `HydEntity' withT* #}
