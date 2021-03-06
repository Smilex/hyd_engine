/**
 * \file entity.h
 */

#ifndef HYD_ENTITY_H
#define HYD_ENTITY_H

#include "sprite.h"
#include <SDL.h>
#include <jansson.h>
#include "vector.h"
#include "property.h"
#include "quad.h"
#include "frame.h"
#include "animation.h"

enum HYD_ENT_COLL {C_NONE = 0x0, C_LEFT = 0x1, C_RIGHT = 0x2, C_TOP = 0x4, C_BOTTOM = 0x8};

struct hyd_layer;
/**
 * \struct hyd_ent
 *
 * An entity is a game object that has a
 * sprite, collision data and a location in the game.
 *
 * Entities are organized in a hierarchy, so that any
 * entity can have a number of children and siblings.
 *
 * For example, an enemy in the game could consist of
 * a root entity that is the body, and two child entities
 * that are its weapons.
 *
 * Each of the entities should be able to move independently,
 * but child entities inherit transformation data from their
 * parents.
 */
struct hyd_ent {
	struct hyd_spr *spr;
	char *name;
	struct hyd_v2 pos;
	struct hyd_ent *parent;

	struct hyd_ent *next;
	struct hyd_ent *prev;
	struct hyd_ent *children;
	struct hyd_property *properties;
	struct hyd_quad coll;
	struct hyd_layer *layer;
	struct hyd_frame *curr_frame;
	struct hyd_anim *curr_anim;
};

/**
 * \param[in] sprite The entity's sprite
 * \param[in] name The entity's name.
 * Pass NULL if you don't intend to search for it
 * \param[in] parent The entity's parent
 * \param[in] children The entity's children
 *
 * \returns Pointer to the new entity. NULL if error.
 */
struct hyd_ent *hyd_ent_create(struct hyd_spr *spr, const char *n,
		struct hyd_ent *parent, struct hyd_layer *layer
		);

/**
 * \brief Creates an entity from a JSON object
 *
 * \param[in] root The JSON object
 * \param[in] parent The entity's parent
 *
 * \return The new entity or NULL on error.
 */
struct hyd_ent *hyd_ent_create_json(json_t *root,
		struct hyd_ent *parent, struct hyd_layer *layer);

/**
 * \brief Creates a entity hierarchy from a JSON array
 *
 * \param[out] ent_list The list to add the created entities to
 * \param[in] root The JSON array
 * \param[in] parent The entity's parent
 * \param[in] renderer The renderer to use for texture loading
 *
 * \return 0 on success, non-zero on failure.
 */
uint8_t hyd_ent_create_json_arr(struct hyd_ent *ent_list, json_t *root,
		struct hyd_ent *parent, struct hyd_layer *layer);

/**
 * \brief Creates an entity from a file
 *
 * \param[in] filename Path to the file to read
 * \param[in] parent The entity's parent
 * \param[in] renderer The renderer to use for texture loading
 *
 * \return The new entity
 */
struct hyd_ent *hyd_ent_create_file(const char *fname,
		struct hyd_ent *parent, struct hyd_layer *layer);

/**
 *
 * \param[in] entity The entity to draw
 */
void hyd_ent_draw(struct hyd_ent *ent);

/**
 *
 * Destroys the entity and its children.
 * Not its siblings.
 *
 * \param[in] entity The entity to destroy
 */
void hyd_ent_destroy(struct hyd_ent *e);

/**
 * \brief Copies the entity
 */
struct hyd_ent *hyd_ent_copy(struct hyd_ent *e);

enum HYD_ENT_COLL hyd_ent_coll(struct hyd_ent *l, struct hyd_ent *r);

/**
 * \brief Finds entities with name
 *
 * \param[in] l The entity list to search through
 * \param[in] name The name to search for
 * \param[out] num The number of entities found
 *
 * \return The entities array with \em name.
 */
struct hyd_ent **hyd_ent_list_find(struct hyd_ent *l, const char *n, uint32_t *num);

struct hyd_ent *hyd_ent_list_find_pos(struct hyd_ent *l, float x, float y);

float hyd_ent_get_number_property(struct hyd_ent *ent, const char *n);

uint8_t hyd_ent_get_bool_property(struct hyd_ent *ent, const char *n);

const char *hyd_ent_get_string_property(struct hyd_ent *ent, const char *n);

void hyd_ent_set_number_property(struct hyd_ent *ent, float value, const char *n);

void hyd_ent_set_bool_property(struct hyd_ent *ent, uint8_t value, const char *n);

void hyd_ent_set_string_property(struct hyd_ent *ent, const char *value, const char *n);

struct hyd_v2 hyd_ent_get_pos(struct hyd_ent *ent);

#endif
