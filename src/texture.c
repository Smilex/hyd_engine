#include "texture.h"
#include "globals.h"
#include <string.h>
#include <physfs.h>

struct hyd_tex *hyd_tex_create(SDL_Texture *ptr, const char *name)
{
	struct hyd_tex *tex = malloc(sizeof(*tex));
	tex->ptr = ptr;
	tex->name = malloc(strlen(name) + 1);
	strcpy(tex->name, name);
	tex->ref_count = malloc(sizeof(uint32_t));
	*tex->ref_count = 0;
	tex->size.x = 0;
	tex->size.y = 0;

	SDL_QueryTexture(ptr, &tex->format, NULL, &tex->size.w, &tex->size.h);

	return tex;
}

struct hyd_tex *hyd_tex_create_file(const char *filename, SDL_Renderer *renderer)
{
	PHYSFS_sint64 file_length = 0;
	uint8_t *buf = NULL;
	SDL_RWops *ops = NULL;
	SDL_Texture *tex;

	if (PHYSFS_exists(filename) == 0) {
		SDL_LogWarn(
				SDL_LOG_CATEGORY_APPLICATION,
				"Could not find texture '%s'",
				filename
				);
		return NULL;
	}

	file_length = hyd_fs_read_buffer(filename, &buf);
	SDL_Log("Reading texture - %s - size: %i", filename, file_length);

	if (file_length == 0) {
		SDL_LogError(
				SDL_LOG_CATEGORY_APPLICATION,
				"Failed to read texture from file: '%s'\n",
				filename
				);
		return NULL;
	}

	ops = SDL_RWFromMem(buf, file_length);
	tex = IMG_LoadTexture_RW(renderer, ops, 0);
	free(buf);
	SDL_RWclose(ops);

	return hyd_tex_create(tex, filename);
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

	(*tex->ref_count) += 1;

	return tex;
}

void hyd_tex_destroy(struct hyd_tex *texture)
{
	if (texture == NULL)
		return;

	if (*(texture->ref_count) == 0) {
		SDL_Log("Destroying texture.");
		SDL_DestroyTexture(texture->ptr);
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

void hyd_tex_draw(struct hyd_tex *tex, SDL_Renderer *rend, SDL_Rect src, SDL_Rect dest) {
	SDL_RenderCopy(rend, tex->ptr, &src, &dest);
}
