/**
 * \file texture.h
 */

#ifndef HYD_TEXTURE_H
#define HYD_TEXTURE_H

#include <SDL.h>
#include <SDL_image.h>
#include "list.h"

/**
 * \struct hyd_tex
 *
 * A texture should represent visual data,
 * either from a file, or memory data.
 */
struct hyd_tex {
	struct SDL_Texture *ptr;
	uint32_t *ref_count;
	char *name;

	struct hyd_list list;
};
typedef struct hyd_tex hyd_tex;

/**
 * \param[in] ptr The texture pointer
 * \param[in] name The texture name
 *
 * \return The new texture
 */
struct hyd_tex *hyd_tex_create(struct SDL_Texture *ptr, const char *name);

/**
 * \brief Creates a new texture using data from the specified file
 *
 * \param[in] filename The path to the file to read from
 * \param[in] renderer The renderer to use for texture loading
 *
 * \return The new texture, or NULL if there was an error
 */
struct hyd_tex *hyd_tex_create_file(const char *filename, struct SDL_Renderer *renderer);

/**
 * \brief Copy a texture and increment the ref count
 *
 * \param[in] texture Texture to copy
 *
 * \return The new copy of the texture
 */
struct hyd_tex *hyd_tex_copy(struct hyd_tex *texture);

/**
 * \param[in] texture The texture to destroy
 */
void hyd_tex_destroy(struct hyd_tex *texture);

/**
 * \param[in] textures The texture list to destroy
 */
void hyd_tex_list_destroy(struct hyd_list *textures);

/**
 * \param[in] textures The texture list to search through
 * \param[in] name The name to search for
 *
 * \return The texture if found, NULL otherwise
 */
struct hyd_tex *hyd_tex_list_find(struct hyd_list *textures, const char *name);

#endif
