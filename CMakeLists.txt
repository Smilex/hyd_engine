cmake_minimum_required (VERSION 2.8)
project (hyd_engine)

option (BUILD_TESTS "Build tests" ON)

set (CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules)

include_directories(include jansson)
file (GLOB SRC src/*.c src/init/*.c src/editor/*.c)

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

find_package(SDL2_image REQUIRED)
if (${SDL_IMAGE_FOUND})
	include_directories(${SDL_IMAGE_INCLUDE_DIRS})
	target_link_libraries(hyd ${SDL_IMAGE_LIBRARIES})
endif ()

find_package(SDL2_ttf REQUIRED)
if (${SDL_TTF_FOUND})
	include_directories(${SDL_TTF_INCLUDE_DIRS})
	target_link_libraries(hyd ${SDL_TTF_LIBRARIES})
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

if (${BUILD_TESTS})
	add_subdirectory("tests")
endif ()
