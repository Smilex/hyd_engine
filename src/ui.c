#include "ui.h"

#include <SDL.h>

enum hyd_btn_state hyd_ui_btn(int x1, int y1, int x2, int y2)
{
	int x, y;
	uint32_t state = SDL_GetMouseState(&x, &y);

	if ((x1 <= x && x2 >= x) &&
		(y1 <= y && y2 >= y)) {
		if (state&SDL_BUTTON(1))
			return DOWN_L;
		else if (state&SDL_BUTTON(2))
			return DOWN_M;
		else if (state&SDL_BUTTON(3))
			return DOWN_R;
		else
			return HOVER;
	}
	else
		return NONE;
}
