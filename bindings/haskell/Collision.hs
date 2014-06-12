{-# LANGUAGE ForeignFunctionInterface #-}

module OpenHydorah.Collision where

import Foreign
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types

foreign import ccall "collision_check" c_collision_check
	:: Ptr (a) -> Ptr (a) -> Ptr (b)

foreign import ccall "collision_list_check" c_collision_list_check
	:: Ptr (a) -> Ptr (b) -> Ptr (c)

foreign import ccall "collision_list_check_list" c_collision_list_check_list
	:: Ptr (a) -> Ptr (a) -> Float -> Float -> Ptr (b)

foreign import ccall "collision_get_intersects" c_collision_get_intersects
	:: Ptr (a) -> CInt

foreign import ccall "collision_get_will_intersect" c_collision_get_will_intersect
	:: Ptr (a) -> CInt

foreign import ccall "collision_get_mtv_x" c_collision_get_mtv_x
	:: Ptr (a) -> Float

foreign import ccall "collision_get_mtv_y" c_collision_get_mtv_y
	:: Ptr (a) -> Float
