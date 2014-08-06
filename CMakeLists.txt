cmake_minimum_required (VERSION 2.8)
project (hyd_engine)

option (BUILD_TESTS "Build tests" OFF)

set (CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules)

include_directories(include jansson/src)
file (GLOB SRC src/*.c src/editor/*.c src/init/*.c)

add_subdirectory(jansson)

add_library(hyd SHARED ${SRC})
target_link_libraries(hyd jansson)

if (NOT WIN32)
	target_link_libraries(hyd -lm)
endif ()

find_package(SDL2 REQUIRED)
if (${SDL_FOUND})
	include_directories(${SDL_INCLUDE_DIR})
	target_link_libraries(hyd ${SDL_LIBRARY})
endif ()

find_package(PhysFS REQUIRED)
if (${PHYSFS_FOUND})
	include_directories(${PHYSFS_INCLUDE_DIR})
	target_link_libraries(hyd ${PHYSFS_LIBRARY})
endif ()

find_package(PNG REQUIRED)
if (${PNG_FOUND})
	include_directories(${PNG_INCLUDE_DIRS})
	target_link_libraries(hyd ${PNG_LIBRARIES})
	add_definitions(${PNG_DEFINITIONS})
endif ()

find_package(OpenGL REQUIRED)
if (${OPENGL_FOUND})
	include_directories(${OPENGL_INCLUDE_DIR})
	target_link_libraries(hyd ${OPENGL_LIBRARIES})
endif ()

if (${BUILD_TESTS})
	add_subdirectory("tests")
endif ()
