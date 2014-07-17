#include "init.h"

int hyd_init_sdl(SDL_Window** window, SDL_GLContext* context,
		const int width, const int height)
{
	if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
		SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
				"Failed to initialize SDL: %s\n",
				SDL_GetError());
		return 1;
	}

	SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);
	SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3);
	SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 3);
	SDL_GL_SetAttribute(SDL_GL_CONTEXT_FLAGS, SDL_GL_CONTEXT_DEBUG_FLAG);
	SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);

	*window = SDL_CreateWindow(
			"OpenHydorah - devel",
			SDL_WINDOWPOS_UNDEFINED,
			SDL_WINDOWPOS_UNDEFINED,
			width, height,
			SDL_WINDOW_OPENGL);

	if (*window == NULL)
	{
		SDL_LogError(SDL_LOG_CATEGORY_VIDEO,
			"Failed to initialize SDL window: %s\n",
			SDL_GetError());
		return 1;
	}

	*context = SDL_GL_CreateContext(*window);

	if (*context == NULL)
	{
		SDL_LogError(SDL_LOG_CATEGORY_VIDEO,
			"Failed to initialize SDL GL context: %s\n",
			SDL_GetError());
		return 1;
	}

	return 0;
}

