/**
 * \file sprite.h
 */

#ifndef HYD_SPRITE_H
#define HYD_SPRITE_H

#include <SDL.h>
#include <stdint.h>
#include "texture.h"
#include "frame.h"
#include "animation.h"
#include <jansson.h>

/**
 * \struct hyd_spr
 *
 * A sprite is a texture with a number
 * of frames and animations.
 */
struct hyd_spr {
	struct hyd_tex *tex;
	struct hyd_frame **frames;
	struct hyd_anim **anims;
	uint32_t num_frames;
	uint32_t num_anims;
};

/**
 * \param[in] texture The sprite's texture
 * \param[in] frames The sprite's frames
 * \param[in] defaultFrame The frame to use by default.
 * If NULL, then it's the first in \em frames
 * \param[in] animations The sprite's animations
 *
 * \return The new sprite or NULL on error.
 */
struct hyd_spr *hyd_spr_create(struct hyd_tex *tex, struct hyd_frame **frames,
		uint32_t num_frames, struct hyd_anim **anims,
		uint32_t num_anims);

/**
 * \brief Creates a sprite from a JSON object
 *
 * \param[in] root The JSON object
 * \param[out] tex_l The list to add the sprite textures to
 * \param[in] renderer The renderer to use for texture loading
 *
 * \return The new sprite or NULL on error
 */
struct hyd_spr *hyd_spr_create_json(json_t *root, struct hyd_tex_list *tex_l,
		struct SDL_Renderer *renderer);

/**
 * \brief Creates a sprite from a file
 *
 * \param[in] filename The file to read from
 * \param[out] tex_l The list to add the sprite textures to
 * \param[in] renderer The renderer to use for texture loading
 *
 * \return The new sprite or NULL on error
 */
struct hyd_spr *hyd_spr_create_file(const char *fname, struct hyd_tex_list *tex_l,
		struct SDL_Renderer *renderer);

/**
 * \param[in] sprite The sprite to destroy
 */
void hyd_spr_destroy(struct hyd_spr *sprite);

/**
 * \brief Draws the sprite at the specified point
 *
 * \param[in] sprite Sprite to draw
 * \param[in] point The point to draw at
 * \param[in] renderer The renderer to use
 */
void hyd_spr_draw_point(struct hyd_spr *sprite, struct SDL_Point point,
		struct SDL_Renderer *renderer);

#endif
