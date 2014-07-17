#include "graphics.h"
#include <assert.h>
#include "filesystem.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

struct hyd_shader *g_shader = NULL;
struct hyd_program *g_program = NULL;
struct hyd_vbo *g_vbo = NULL;
struct hyd_ebo *g_ebo = NULL;

struct hyd_program *g_bprogram = NULL;
struct hyd_vbo *g_bvbo = NULL;
struct hyd_ebo *g_bebo = NULL;

void hyd_shader_impl(void) {
	assert(g_shader == NULL);

	g_shader = malloc(sizeof(*g_shader));
}

void hyd_shader_type(GLenum type) {
	assert(g_shader != NULL);
	g_shader->ptr = glCreateShader(type);
}

void hyd_shader_source(const char *str) {
	assert(g_shader != NULL);
	glShaderSource(g_shader->ptr, 1, &str, NULL);
}

void hyd_shader_source_file(const char *fname) {
	assert(g_shader != NULL);

	PHYSFS_sint64 read_len;
	uint8_t *buf;

	read_len = hyd_fs_read_buffer(fname, &buf);
	assert(read_len != 0);

	glShaderSource(g_shader->ptr, 1, ((const GLchar**)&buf), (GLint*)&read_len);
}

struct hyd_shader *hyd_shader_finish(void) {
	assert(g_shader != NULL);
	glCompileShader(g_shader->ptr);

	GLint status;

	glGetShaderiv(g_shader->ptr, GL_COMPILE_STATUS, &status);
	if (status == GL_FALSE) {
		char buffer[512];
		glGetShaderInfoLog(g_shader->ptr, 512, NULL, buffer);
		printf("Shader info:\n%s\n", buffer);
	}

	struct hyd_shader *r = g_shader;
	g_shader = NULL;

	return r;
}

struct hyd_shader *hyd_shader_create_file(GLenum type, const char *fname) {
	hyd_shader_impl();
		hyd_shader_type(type);
		hyd_shader_source_file(fname);

	return hyd_shader_finish();
}

void hyd_program_impl(void) {
	assert(g_program == NULL);
	g_program = malloc(sizeof(*g_program));
	g_program->ptr = glCreateProgram();
	g_program->attrib = NULL;
	g_program->num_attrs = 0;
}

void hyd_program_attach(struct hyd_shader *s) {
	assert(g_program != NULL);
	glAttachShader(g_program->ptr, s->ptr);
}

void hyd_program_attrib_ptr(const char *n) {
	assert(g_program != NULL);

	struct hyd_program *p = g_program;
	
	if (p->attrib == NULL)
		p->attrib = malloc(sizeof(*p->attrib));
	else {
		p->attrib = realloc(p->attrib,
				sizeof(*p->attrib) * (p->num_attrs + 1));
	}

	p->attrib[p->num_attrs] = malloc(sizeof(*p->attrib) + 1);
	strcpy(p->attrib[p->num_attrs],n);
	p->num_attrs++;
}

void hyd_program_bind_attrib(const char *name, int num) {
	assert(g_program != NULL);

	glBindAttribLocation(g_program->ptr, num, name);
}

struct hyd_program *hyd_program_finish(void) {
	assert(g_program != NULL);
	glBindFragDataLocation(g_program->ptr, 0, "outColor");
	glLinkProgram(g_program->ptr);

	GLint status;
	glGetProgramiv(g_program->ptr, GL_LINK_STATUS, &status);

	if (status == GL_FALSE) {
		char buffer[512];
		glGetShaderInfoLog(g_program->ptr, 512, NULL, buffer);
		printf("%s\n", buffer);
	}

	struct hyd_program *r = g_program;
	g_program = NULL;

	return r;
}

void hyd_program_use(struct hyd_program *p) {
	if (g_bprogram != NULL && p->ptr == g_bprogram->ptr)
		return;

	glUseProgram(p->ptr);
	g_bprogram = p;

	if (g_bvbo != NULL && g_bvbo->ptr != 0)
		hyd_program_proc_ab(p);
}

void hyd_program_proc_ab(struct hyd_program *p) {
	uint32_t i;
	GLint posAttrib;
	for (i = 0; i < p->num_attrs; i++) {
		posAttrib = glGetAttribLocation(p->ptr, p->attrib[i]);
		glEnableVertexAttribArray(posAttrib);
		glVertexAttribPointer(posAttrib, 2, GL_FLOAT, GL_FALSE, 0, 0);
	}
}

struct hyd_program *hyd_program_bound(void) {
	return g_bprogram;
}

void hyd_vbo_impl(void) {
	assert(g_vbo == NULL);

	g_vbo = malloc(sizeof(*g_vbo));
	glGenBuffers(1, &g_vbo->ptr);
	glBindBuffer(GL_ARRAY_BUFFER, g_vbo->ptr);
}

void hyd_vbo_data(GLfloat *vertices, GLuint len) {
	assert(g_vbo != NULL);

	glBufferData(GL_ARRAY_BUFFER, len, vertices, GL_STATIC_DRAW);
}

struct hyd_vbo *hyd_vbo_finish(void) {
	assert(g_vbo != NULL);
	glBindBuffer(GL_ARRAY_BUFFER, 0);
	struct hyd_vbo *r = g_vbo;
	g_vbo = NULL;

	return r;
}

void hyd_vbo_bind(struct hyd_vbo *vbo) {
	if (g_bvbo != NULL && g_bvbo->ptr == vbo->ptr)
		return;

	glBindBuffer(GL_ARRAY_BUFFER, vbo->ptr);
	g_bvbo = vbo;

	if (g_bprogram != NULL && g_bprogram->ptr != 0)
		hyd_program_proc_ab(g_bprogram);
}

void hyd_ebo_impl(void) {
	assert(g_ebo == NULL);

	g_ebo = malloc(sizeof(*g_ebo));
	glGenBuffers(1, &g_ebo->ptr);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, g_ebo->ptr);
}

void hyd_ebo_data(GLuint *vertices, GLuint len) {
	assert(g_ebo != NULL);

	glBufferData(GL_ELEMENT_ARRAY_BUFFER, len, vertices, GL_STATIC_DRAW);
}

struct hyd_ebo *hyd_ebo_finish(void) {
	assert(g_ebo != NULL);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
	struct hyd_ebo *r = g_ebo;
	g_ebo = NULL;

	return r;
}

void hyd_ebo_bind(struct hyd_ebo *ebo) {
	if (g_bebo != NULL && g_bebo->ptr == ebo->ptr)
		return;

	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ebo->ptr);
	g_bebo = ebo;

	if (g_bprogram != NULL && g_bprogram->ptr != 0)
		hyd_program_proc_ab(g_bprogram);
}

void hyd_gfx_draw(GLenum ty, uint32_t n) {
	assert (g_bvbo != 0);
	glDrawArrays(ty, 0, n);
}

void hyd_gfx_draw_inst(void) {
	assert (g_bebo != 0);
	glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);
}
