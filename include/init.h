/**
 * \file init.h
 * Contains functions for engine initialization
 */

#ifndef HYD_INIT_H
#define HYD_INIT_H

#include <SDL.h>
#include <physfs.h>

/**
 * \brief Initializes SDL
 *
 * \param[out] window Returns the created window
 * \param[out] renderer Returns the created renderer
 * \param[in] width The window width
 * \param[in] height The window height
 *
 * \return Non zero if there was an error.
 */
int hyd_init_sdl(SDL_Window **window, SDL_GLContext *context,
		const int width, const int height);

#endif
