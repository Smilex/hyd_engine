/**
 * \file ui.h
 */

#ifndef HYD_ENGINE_H
#define HYD_ENGINE_H

enum hyd_btn_state {
	NONE, HOVER, DOWN_L, DOWN_M, DOWN_R
};

enum hyd_btn_state hyd_ui_btn(int x1, int y1, int x2, int y2);

#endif
