/**
 * \file editor.h
 */

#ifndef HYD_EDITOR_H
#define HYD_EDITOR_H

#include <stdint.h>
#include "quad.h"
#include "graphics.h"
#include "text.h"
#include "transform.h"
#include "scene.h"

/**
 * \struct hyd_editor
 */
struct hyd_editor {
	struct hyd_program *argb_shdr;
	struct hyd_program *text_shdr;
	struct hyd_quad area;
	struct hyd_font *font;
	struct hyd_locale *locale;
	struct hyd_transform *view;
	struct hyd_scene *scene;
	uint8_t screen_drag, ent_drag;
	uint32_t last_state;
};

uint8_t hyd_editor_init(struct hyd_transform *view, struct hyd_scene *scene);

struct hyd_editor *
hyd_editor_create(struct hyd_quad area, struct hyd_program *argb, struct hyd_program *text,
		struct hyd_font *font, struct hyd_locale *locale, struct hyd_transform *view);

uint8_t hyd_editor_open();

uint8_t hyd_editor_update(void);

void hyd_editor_draw();

void hyd_editor_ui(void);

#endif
