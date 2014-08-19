#include "editor.h"
#include <stdlib.h>
#include "transform.h"
#include <SDL.h>
#include "ui.h"
#include "entity.h"
#include <assert.h>
#include <stdio.h>

struct hyd_editor *_ed_editor;
struct hyd_ent *_ed_selected[50];
uint32_t _ed_num_selected = 0;

uint8_t hyd_editor_init(struct hyd_transform *view, struct hyd_scene *scene) {
	_ed_editor = malloc(sizeof(*_ed_editor));
	if (_ed_editor == NULL)
		return 1;

	_ed_editor->view = view;
	_ed_editor->scene = scene;
	_ed_editor->screen_drag = 0;
	_ed_editor->ent_drag = 0;
	memset(_ed_selected, 0, sizeof(_ed_selected));

	return 0;
}

struct hyd_editor *
hyd_editor_create(struct hyd_quad area, struct hyd_program *argb, struct hyd_program *text,
		struct hyd_font *font, struct hyd_locale *locale, struct hyd_transform *view) {
	struct hyd_editor *e = (struct hyd_editor*)malloc(sizeof(*e));
	if (e == NULL)
		return NULL;

	e->area = area;
	e->argb_shdr = argb;
	e->text_shdr = text;
	e->font = font;
	e->view = view;
	e->locale = locale;
	hyd_locale_render(e->locale, e->font);


	return e;
}

uint8_t hyd_editor_open() {

	return 0;
}

uint8_t hyd_editor_update(void) {
	int r_x, r_y, m_x, m_y, m_scroll, i;
	uint32_t m_state;
	uint8_t m_button;
	struct hyd_editor *e = _ed_editor;
	struct hyd_v2 r_v, m_v, t_v;
	struct hyd_transform m_trans = *e->view;
	m_trans.mat[12] = -m_trans.mat[12];
	m_trans.mat[13] = -m_trans.mat[13];

	SDL_GetRelativeMouseState(&r_x, &r_y);
	m_state = SDL_GetMouseState(&m_x, &m_y);

	m_v.x = (float)m_x; m_v.y = (float)m_y;
	r_v.x = (float)r_x; r_v.y = (float)r_y;
	t_v = hyd_transform_mul_v2(&m_trans, &m_v);

	if (e->ent_drag) {
		for (i = 0; i < _ed_num_selected; i++) {
			_ed_selected[i]->pos = hyd_v2_add(r_v, _ed_selected[i]->pos);
		}
	}
	else if (e->screen_drag)
		hyd_transform_translate(e->view, r_v.x, r_v.y, 0);

	if (m_state & SDL_BUTTON(SDL_BUTTON_LEFT)) {
		struct hyd_ent *sel_ent = hyd_ent_list_find_pos(e->scene->layer_head->next->ent_head, t_v.x, t_v.y);
		if (sel_ent != NULL) {
			assert(_ed_num_selected < 50);
			uint8_t found = 0;
			for (i = 0; i < _ed_num_selected; i++) {
				if (_ed_selected[i] == sel_ent) {
					found = 1;
					break;
				}
			}
			
			if (!found) {
				_ed_selected[_ed_num_selected] = sel_ent;
				_ed_num_selected++;
			}
		}
		else
			_ed_num_selected = 0;

		if (_ed_num_selected > 0) {
			e->ent_drag = 1;
		}
		else {
			e->screen_drag = 1;
			e->ent_drag = 0;
		}
	}
	else {
		if (e->last_state & SDL_BUTTON(SDL_BUTTON_LEFT)) {
		}
		else {
			if (e->screen_drag)
				e->screen_drag = 0;
			if (e->ent_drag)
				e->ent_drag = 0;
		}
	}
	
	e->last_state = m_state;
	return 0;
}

void hyd_editor_draw() {
	uint32_t i;

	for (i = 0; i < _ed_num_selected; i++) {
		struct hyd_quad q;
		struct hyd_color c = {
			0, 1.0f, 0, 1.0f
		};
		struct hyd_ent *s = _ed_selected[i];
		q.x1 = s->pos.x;
		q.y1 = s->pos.y;
		q.x2 = q.x1 + s->spr->frames[0]->rect.w;
		q.y2 = q.y1 + s->spr->frames[0]->rect.h;

		hyd_quad_draw(&q, &c);
	}
}

void hyd_editor_ui(void) {
}
