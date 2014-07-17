/**
 * \file graphics.h
 */

#ifndef HYD_GRAPHICS_H
#define HYD_GRAPHICS_H

#include "gl_core_3_3.h"
#include <stdint.h>

struct hyd_shader {
	GLuint ptr;
};

struct hyd_program {
	GLuint ptr;
	GLchar **attrib;
	GLuint num_attrs;
};

struct hyd_vbo {
	GLuint ptr;
};

struct hyd_ebo {
	GLuint ptr;
};

/* SHADER */

void hyd_shader_impl(void);

void hyd_shader_type(GLenum type);

void hyd_shader_source(const char *str);

void hyd_shader_source_file(const char *fname);

struct hyd_shader *hyd_shader_finish(void);

struct hyd_shader *hyd_shader_create_file(GLenum type, const char *fname);

/* SHADER PROGRAM */

void hyd_program_impl(void);

void hyd_program_attach(struct hyd_shader *s);

void hyd_program_attrib_ptr(const char *n);

void hyd_program_bind_attrib(const char *name, int num);

struct hyd_program *hyd_program_finish(void);

void hyd_program_use(struct hyd_program *p);

void hyd_program_proc_ab(struct hyd_program *p);

struct hyd_program *hyd_program_bound(void);

/* VERTEX BUFFER */

void hyd_vbo_impl(void);

void hyd_vbo_data(GLfloat *data, GLuint len);

struct hyd_vbo *hyd_vbo_finish(void);

void hyd_vbo_bind(struct hyd_vbo *vbo);

/* ELEMENT BUFFER */
void hyd_ebo_impl(void);

void hyd_ebo_data(GLuint *data, GLuint len);

struct hyd_ebo *hyd_ebo_finish(void);

void hyd_ebo_bind(struct hyd_ebo *ebo);

/* GENERAL */
void hyd_gfx_draw(GLenum ty, uint32_t n);

void hyd_gfx_draw_inst(void);

#endif
