set(SRC tests_main.c tests_texture.c tests_entity.c tests_sprite.c tests_property.c
	tests_collision.c)
file (GLOB OH_SRC ../src/*.c ../src/init/*.c ../src/editor/*.c)

include_directories(../include)

add_executable(hydorah_tests ${OH_SRC} ${SRC})
target_link_libraries(hydorah_tests jansson -lm)

find_package(Check REQUIRED)
if (${CHECK_FOUND})
	include_directories(${CHECK_INCLUDE_DIR})
	target_link_libraries(hydorah_tests ${CHECK_LIBRARIES})
endif ()

find_package(SDL2 REQUIRED)
if (${SDL_FOUND})
	include_directories(${SDL_INCLUDE_DIR})
	target_link_libraries(hydorah_tests ${SDL_LIBRARY})
endif ()

find_package(PhysFS REQUIRED)
if (${PHYSFS_FOUND})
	include_directories(${PHYSFS_INCLUDE_DIR})
	target_link_libraries(hydorah_tests ${PHYSFS_LIBRARY})
endif ()

find_package(OpenGL REQUIRED)
if (${OPENGL_FOUND})
	include_directories(${OPENGL_INCLUDE_DIR})
	target_link_libraries(hydorah_tests ${OPENGL_LIBRARIES})
endif ()

