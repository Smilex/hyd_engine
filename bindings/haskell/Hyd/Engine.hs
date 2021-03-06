-- GENERATED by C->Haskell Compiler, version 0.16.4 Crystal Seed, 24 Jan 2009 (Haskell)
-- Edit the ORIGNAL .chs file instead!


{-# LINE 1 "Engine.chs" #-}{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}


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
	sizeOf _ = 96
{-# LINE 27 "Engine.chs" #-}
	alignment _ = 8
{-# LINE 28 "Engine.chs" #-}
	peek p = HydEntity
			<$> ((\ptr -> do {peekByteOff ptr 0 ::IO CUChar}) p)
			<*> ((\ptr -> do {peekByteOff ptr 8 ::IO (Ptr ())}) p)
			<*> ((\ptr -> do {peekByteOff ptr 40 ::IO (Ptr ())}) p)
			<*> ((\ptr -> do {peekByteOff ptr 56 ::IO (Ptr ())}) p)
	poke p x = do
		(\ptr val -> do {pokeByteOff ptr 0 (val::CUChar)}) p (running'hyd_engine x)
		(\ptr val -> do {pokeByteOff ptr 8 (val::(Ptr ()))}) p (renderer'hyd_engine x)
		(\ptr val -> do {pokeByteOff ptr 40 (val::(Ptr ()))}) p (current_scene'hyd_engine x)
		(\ptr val -> do {pokeByteOff ptr 56 (val::(Ptr ()))}) p (current_input_preset'hyd_engine x)

type HydEntityPtr = Ptr (HydEntity)
{-# LINE 40 "Engine.chs" #-}

withT = with

hydEngineCreate :: IO (HydEngine)
hydEngineCreate =
  hydEngineCreate'_ >>= \res ->
  withT res >>= \res' ->
  return (res')
{-# LINE 45 "Engine.chs" #-}

hydEngineInit :: HydEngine -> Ptr (String) -> IO (Bool)
hydEngineInit a1 a2 =
  withT a1 $ \a1' -> 
  let {a2' = id a2} in 
  hydEngineInit'_ a1' a2' >>= \res ->
  let {res' = toBool res} in
  return (res')
{-# LINE 50 "Engine.chs" #-}

hydEngineDestroy :: HydEngine -> IO ()
hydEngineDestroy a1 =
  withT a1 $ \a1' -> 
  hydEngineDestroy'_ a1' >>= \res ->
  return ()
{-# LINE 54 "Engine.chs" #-}

hydEngineRun :: HydEngine -> IO (Bool)
hydEngineRun a1 =
  withT a1 $ \a1' -> 
  hydEngineRun'_ a1' >>= \res ->
  let {res' = toBool res} in
  return (res')
{-# LINE 58 "Engine.chs" #-}

hydEngineLoadScene :: HydEngine -> String -> IO (Bool)
hydEngineLoadScene a1 a2 =
  withT a1 $ \a1' -> 
  withCString a2 $ \a2' -> 
  hydEngineLoadScene'_ a1' a2' >>= \res ->
  let {res' = toBool res} in
  return (res')
{-# LINE 63 "Engine.chs" #-}

hydEngineLoadInputPreset :: HydEngine -> String -> IO (Bool)
hydEngineLoadInputPreset a1 a2 =
  withT a1 $ \a1' -> 
  withCString a2 $ \a2' -> 
  hydEngineLoadInputPreset'_ a1' a2' >>= \res ->
  let {res' = toBool res} in
  return (res')
{-# LINE 68 "Engine.chs" #-}

foreign import ccall safe "Engine.chs.h hyd_engine_create"
  hydEngineCreate'_ :: (IO (Ptr ()))

foreign import ccall safe "Engine.chs.h hyd_engine_init"
  hydEngineInit'_ :: ((Ptr ()) -> ((Ptr (Ptr CChar)) -> (IO CUChar)))

foreign import ccall safe "Engine.chs.h hyd_engine_destroy"
  hydEngineDestroy'_ :: ((Ptr ()) -> (IO ()))

foreign import ccall safe "Engine.chs.h hyd_engine_run"
  hydEngineRun'_ :: ((Ptr ()) -> (IO CUChar))

foreign import ccall safe "Engine.chs.h hyd_engine_load_scene"
  hydEngineLoadScene'_ :: ((Ptr ()) -> ((Ptr CChar) -> (IO CUChar)))

foreign import ccall safe "Engine.chs.h hyd_engine_load_input_preset"
  hydEngineLoadInputPreset'_ :: ((Ptr ()) -> ((Ptr CChar) -> (IO CUChar)))
