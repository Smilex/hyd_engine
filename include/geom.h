/**
 * \file geom.h
 */

#ifndef HYD_GEOM_H
#define HYD_GEOM_H

#include <SDL.h>
#include "graphics.h"

void hyd_hex_color(uint32_t hex, uint8_t *r, uint8_t *g, uint8_t *b);

void hyd_geom_draw_line(SDL_Point p1, SDL_Point p2,
		uint32_t hex, uint8_t opacity);

void hyd_geom_draw_rect(SDL_Rect rect,
		uint32_t hex, uint8_t opacity, uint8_t fill);

#endif
