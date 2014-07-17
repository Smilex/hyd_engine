/**
 * \file scene.h
 */

#ifndef HYD_SCENE_H
#define HYD_SCENE_H

#include "texture.h"
#include "entity.h"
#include "sprite.h"

/**
 * \struct hyd_scene
 *
 * A scene contains a collection of entities and sprites
 */
struct hyd_scene {
	struct hyd_ent *ent_head;
};

/**
 * \return The new scene
 */
struct hyd_scene *hyd_scene_create(void);

/**
 * \brief Creates a new scene from a JSON object
 *
 * \param[in] root The JSON object
 * \param[out] tex_l The list to add the sprites' textures
 *
 * \return The new scene
 */
struct hyd_scene *hyd_scene_create_json(json_t *root, struct hyd_tex_list *tex_l);

/**
 * \brief Creates a new scene from file
 *
 * \param[in] filename Path to file to read
 * \param[out] tex_l The list to add the sprites' textures
 *
 * \return The new scene
 */
struct hyd_scene *hyd_scene_create_file(const char *fname, struct hyd_tex_list *tex_l);

/**
 * \brief Destroys the scene and its objects
 *
 * \param[in] scene The scene to destroy
 */
void hyd_scene_destroy(struct hyd_scene *s);

/**
 * \brief Draws the scene
 *
 * This draws everything in the scene
 *
 * \param[in] scene The scene to draw
 */
void hyd_scene_draw(struct hyd_scene *s);

#endif
