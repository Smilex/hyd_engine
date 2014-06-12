{-# LANGUAGE ForeignFunctionInterface #-}

module OpenHydorah.Engine where

import Foreign
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types

foreign import ccall "engine_create" c_engine_create
	:: Ptr (a)

foreign import ccall "engine_destroy" c_engine_destroy
	:: Ptr (a) -> IO ()

foreign import ccall "engine_run" c_engine_run
	:: Ptr (a) -> IO ()

foreign import ccall "engine_init" c_engine_init
	:: Ptr (a) -> Ptr (b) -> IO ()

foreign import ccall "engine_get_current_scene" c_engine_get_current_scene
	:: Ptr (a) -> Ptr (b)

foreign import ccall "engine_get_current_input_preset" c_engine_get_current_input_preset
	:: Ptr (a) -> Ptr (b)

foreign import ccall "engine_load_scene" c_engine_load_scene
	:: Ptr (a) -> CString -> IO ()

foreign import ccall "engine_load_input_preset" c_engine_load_input_preset
	:: Ptr (a) -> CString -> IO ()

foreign import ccall "engine_get_renderer" c_engine_get_renderer
	:: Ptr (a) -> Ptr (b)
