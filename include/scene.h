/**
 * \file scene.h
 */

#ifndef HYD_SCENE_H
#define HYD_SCENE_H

#include "texture.h"
#include "list.h"
#include "entity.h"
#include "sprite.h"

/**
 * \struct hyd_scene
 *
 * A scene contains a collection of entities and sprites
 */
struct hyd_scene {
	struct hyd_list entities;
	struct hyd_list sprites;
};
typedef struct hyd_scene hyd_scene;

/**
 * \return The new scene
 */
struct hyd_scene *hyd_scene_create(void);

/**
 * \brief Creates a new scene from a JSON object
 *
 * \param[in] root The JSON object
 * \param[out] textures The list to add the sprites' textures
 * \param[in] renderer The renderer to use for texture loading
 *
 * \return The new scene
 */
struct hyd_scene *hyd_scene_create_json(json_t *root, struct hyd_list *textures,
		struct SDL_Renderer *renderer);

/**
 * \brief Creates a new scene from file
 *
 * \param[in] filename Path to file to read
 * \param[out] textures The list to add the sprites' textures
 * \param[in] renderer The renderer to use for texture loading
 *
 * \return The new scene
 */
struct hyd_scene *hyd_scene_create_file(const char *filename, struct hyd_list *textures,
		struct SDL_Renderer *renderer);

/**
 * \brief Destroys the scene and its objects
 *
 * \param[in] scene The scene to destroy
 */
void hyd_scene_destroy(struct hyd_scene *scene);

/**
 * \brief Draws the scene
 *
 * This draws everything in the scene
 *
 * \param[in] scene The scene to draw
 * \param[in] renderer The renderer to use
 */
void hyd_scene_draw(struct hyd_scene *scene, struct SDL_Renderer *renderer);

/* GETTERS */
struct hyd_list *hyd_scene_get_entities(struct hyd_scene *scene);

#endif
