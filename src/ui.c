#include "ui.h"

enum HYD_UI_BUTTON_STATE
hyd_ui_button(struct hyd_quad q) {
	int m_x, m_y;
	uint32_t m_ret;
	enum HYD_UI_BUTTON_STATE state = NONE;

	m_ret = SDL_GetMouseState(&m_x, &m_y);

	if (m_x > q.x1 && m_x < q.x2 && m_y > q.y1 && m_y < q.y2) {
		if (m_ret & SDL_BUTTON(SDL_BUTTON_LEFT))
			state = L_DOWN;
		else if (m_ret & SDL_BUTTON(SDL_BUTTON_RIGHT))
			state = R_DOWN;
		else if (m_ret & SDL_BUTTON(SDL_BUTTON_MIDDLE))
			state = M_DOWN;
		else
			state = HOVER;
	}

	return state;
}

