#include "texture.h"
#include "globals.h"
#include <string.h>
#include <physfs.h>
#include "graphics.h"
#include "matrix.h"

#define STB_IMAGE_IMPLEMENTATION
#define STBI_NO_STDIO
#include "stb_image.h"

struct hyd_tex *hyd_tex_create(const char *name)
{
	struct hyd_tex *tex = malloc(sizeof(*tex));
	if (tex == NULL)
		return NULL;

	tex->name = malloc(strlen(name) + 1);
	if (tex->name == NULL)
		return NULL;
	strcpy(tex->name, name);
	tex->ref_count = malloc(sizeof(uint32_t));
	*tex->ref_count = 0;
	tex->w = 0;
	tex->h = 0;

	glGenTextures(1, &tex->ptr);

	return tex;
}

struct hyd_tex *hyd_tex_create_file(const char *filename)
{
	PHYSFS_sint64 read_len = 0;
	uint8_t *buf, *data;
	struct hyd_tex *tex;
	int w, h, comp;

	if (PHYSFS_exists(filename) == 0) {
		SDL_LogWarn(
				SDL_LOG_CATEGORY_APPLICATION,
				"Could not find texture '%s'",
				filename
				);
		return NULL;
	}

	read_len = hyd_fs_read_buffer(filename, &buf);
	SDL_Log("Reading texture - %s - size: %i", filename, read_len);

	if (read_len == 0) {
		SDL_LogError(
				SDL_LOG_CATEGORY_APPLICATION,
				"Failed to read texture from file: '%s'\n",
				filename
				);
		return NULL;
	}

	data = stbi_load_from_memory(buf, read_len, &w, &h, &comp, 4);
	free(buf);

	if (data == NULL || comp != 4)
		return NULL;

	tex = hyd_tex_create(filename);

	hyd_tex_data(tex, data, w, h);
	free(data);

	return tex;
}

struct hyd_tex *hyd_tex_copy(struct hyd_tex *texture)
{
	if (texture == NULL) {
		SDL_LogError(
				SDL_LOG_CATEGORY_APPLICATION,
				"Trying to copy NULL pointer as texture"
				);
		return NULL;
	}

	struct hyd_tex *tex = malloc(sizeof(*tex));
	tex->ptr = texture->ptr;
	tex->ref_count = texture->ref_count;
	tex->name = texture->name;
	tex->w = texture->w;
	tex->h = texture->h;

	(*tex->ref_count) += 1;

	return tex;
}

void hyd_tex_destroy(struct hyd_tex *texture)
{
	if (texture == NULL)
		return;

	if (*(texture->ref_count) == 0) {
		SDL_Log("Destroying texture.");
		glDeleteTextures(1, &texture->ptr);
		free(texture->ref_count);
		free(texture->name);
	} else
		*(texture->ref_count) -= 1;

	free(texture);
}

void hyd_tex_list_destroy(struct hyd_tex_list *l)
{
	if (l == NULL)
		return;

	struct hyd_tex_list *i, *n;
	for (i = l->next, n = i->next;
			i != l;
			i = n, n = i->next)
	{
		hyd_tex_destroy(i->tex);
		free(i);
	}
}

struct hyd_tex *hyd_tex_list_find(struct hyd_tex_list *l, const char *n)
{
	struct hyd_tex_list *i;
	for (i = l->next; i != l; i = i->next)
	{
		if (strcmp(i->tex->name, n) == 0)
			return i->tex;
	}

	return NULL;
}

void hyd_tex_data(struct hyd_tex *tex, uint8_t *data, int w, int h) {
	glBindTexture(GL_TEXTURE_2D, tex->ptr);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, data);

	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glBindTexture(GL_TEXTURE_2D, 0);

	tex->w = w;
	tex->h = h;
}
