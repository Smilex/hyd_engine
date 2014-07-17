/**
 * \file texture.h
 */

#ifndef HYD_TEXTURE_H
#define HYD_TEXTURE_H

#include <SDL.h>
#include <stdint.h>

#include "gl_core_3_3.h"

/**
 * \struct hyd_tex
 *
 * A texture should represent visual data,
 * either from a file, or memory data.
 */
struct hyd_tex {
	GLuint ptr;
	uint32_t *ref_count;
	char *name;
	int w, h;
};

/**
 * \struct hyd_tex_list
 *
 * A list of textures
 */
struct hyd_tex_list {
	struct hyd_tex *tex;
	struct hyd_tex_list *next;
};

/**
 * \param[in] ptr The texture pointer
 * \param[in] name The texture name
 *
 * \return The new texture
 */
struct hyd_tex *hyd_tex_create(const char *name);

/**
 * \brief Creates a new texture using data from the specified file
 *
 * \param[in] filename The path to the file to read from
 * \param[in] renderer The renderer to use for texture loading
 *
 * \return The new texture, or NULL if there was an error
 */
struct hyd_tex *hyd_tex_create_file(const char *filename);

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
void hyd_tex_list_destroy(struct hyd_tex_list *l);

/**
 * \param[in] textures The texture list to search through
 * \param[in] name The name to search for
 *
 * \return The texture if found, NULL otherwise
 */
struct hyd_tex *hyd_tex_list_find(struct hyd_tex_list *l, const char *n);

void hyd_tex_data(struct hyd_tex *tex, uint8_t *data, int w, int h);

#endif
