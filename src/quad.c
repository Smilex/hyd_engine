#include "quad.h"
#include "gl_core_3_3.h"
#include "graphics.h"
#include "transform.h"

GLuint _hyd_quad_vao[2] = {0};
GLuint _hyd_quad_vbos[3] = {0};

static void hyd_gen_quad_vao() {
	glGenVertexArrays(2, _hyd_quad_vao);
	glBindVertexArray(_hyd_quad_vao[0]);
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);

	glBindVertexArray(_hyd_quad_vao[1]);
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	glEnableVertexAttribArray(2);
}

static void hyd_gen_quad_vbos() {
	glGenBuffers(3, _hyd_quad_vbos);
}

void hyd_quad_draw(struct hyd_quad *q, struct hyd_color *c) {
	if (_hyd_quad_vao[0] == 0)
		hyd_gen_quad_vao();
	if (_hyd_quad_vbos[0] == 0)
		hyd_gen_quad_vbos();
	glBindVertexArray(_hyd_quad_vao[0]);

	const GLfloat verts[] = {
		q->x1, q->y1,
		q->x2, q->y1,
		q->x1, q->y2,
		q->x2, q->y1,
		q->x2, q->y2,
		q->x1, q->y2
	};
	const GLfloat cols[] = {
		c->r, c->g, c->b, c->a,
		c->r, c->g, c->b, c->a,
		c->r, c->g, c->b, c->a,
		c->r, c->g, c->b, c->a,
		c->r, c->g, c->b, c->a,
		c->r, c->g, c->b, c->a
	};

	glBindBuffer(GL_ARRAY_BUFFER, _hyd_quad_vbos[0]);
	glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, sizeof(GL_FLOAT) * 2, (void*)0);
	glBufferData(GL_ARRAY_BUFFER, 12 * sizeof(GLfloat), verts, GL_STATIC_DRAW);
	glBindBuffer(GL_ARRAY_BUFFER, _hyd_quad_vbos[1]);
	glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, sizeof(GL_FLOAT) * 4, (void*)0);
	glBufferData(GL_ARRAY_BUFFER, 24 * sizeof(GLfloat), cols, GL_STATIC_DRAW);

	struct hyd_program *prog = hyd_program_bound();
	if (prog != NULL) {
		GLint loc = glGetUniformLocation(prog->ptr, "model_mat");
		if (loc != -1) {
			struct hyd_transform tr = hyd_transform_get_applied();
			glUniformMatrix4fv(loc, 1, GL_FALSE, tr.mat);
		}
	}

	glDrawArrays(GL_TRIANGLES, 0, 6);
}

void hyd_quad_tex_draw(struct hyd_quad *q, struct hyd_color *c,
		struct hyd_tex *t, struct hyd_quad *uv) {
	if (_hyd_quad_vao[0] == 0)
		hyd_gen_quad_vao();
	if (_hyd_quad_vbos[0] == 0)
		hyd_gen_quad_vbos();
	glBindVertexArray(_hyd_quad_vao[1]);

	const GLfloat verts[] = {
		q->x1, q->y1,
		q->x2, q->y1,
		q->x1, q->y2,
		q->x2, q->y1,
		q->x2, q->y2,
		q->x1, q->y2
	};
	const GLfloat cols[] = {
		c->r, c->g, c->b, c->a,
		c->r, c->g, c->b, c->a,
		c->r, c->g, c->b, c->a,
		c->r, c->g, c->b, c->a,
		c->r, c->g, c->b, c->a,
		c->r, c->g, c->b, c->a
	};
	const GLfloat uvs[] = {
		uv->x1, uv->y1,
		uv->x2, uv->y1,
		uv->x1, uv->y2,
		uv->x2, uv->y1,
		uv->x2, uv->y2,
		uv->x1, uv->y2
	};

	glBindBuffer(GL_ARRAY_BUFFER, _hyd_quad_vbos[0]);
	glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, sizeof(GL_FLOAT) * 2, (void*)0);
	glBufferData(GL_ARRAY_BUFFER, 12 * sizeof(GLfloat), verts, GL_STATIC_DRAW);
	glBindBuffer(GL_ARRAY_BUFFER, _hyd_quad_vbos[1]);
	glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, sizeof(GL_FLOAT) * 4, (void*)0);
	glBufferData(GL_ARRAY_BUFFER, 24 * sizeof(GLfloat), cols, GL_STATIC_DRAW);
	glBindBuffer(GL_ARRAY_BUFFER, _hyd_quad_vbos[2]);
	glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, sizeof(GL_FLOAT) * 2, (void*)0);
	glBufferData(GL_ARRAY_BUFFER, 12 * sizeof(GLfloat), uvs, GL_STATIC_DRAW);

	struct hyd_program *prog = hyd_program_bound();
	if (prog != NULL) {
		GLint loc = glGetUniformLocation(prog->ptr, "model_mat");
		if (loc != -1) {
			struct hyd_transform tr = hyd_transform_get_applied();
			glUniformMatrix4fv(loc, 1, GL_FALSE, tr.mat);
		}
	}

	glBindTexture(GL_TEXTURE_2D, t->ptr);
	glDrawArrays(GL_TRIANGLES, 0, 6);
}
