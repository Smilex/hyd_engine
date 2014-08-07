/**
 * \file ui.h
 */

#ifndef HYD_UI_H
#define HYD_UI_H

#include <stdint.h>
#include "quad.h"
#include "color.h"

enum HYD_UI_BUTTON_STATE {
	NONE, HOVER, L_DOWN, R_DOWN, M_DOWN
};

enum HYD_UI_BUTTON_STATE
hyd_ui_button(struct hyd_quad q);

#endif

