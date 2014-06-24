/**
 * \file collision.h
 */

#ifndef HYD_COLLISION_H
#define HYD_COLLISION_H

#include <stdint.h>
#include <jansson.h>
#include <SDL.h>
#include "vector.h"

enum hyd_coll_type {
	POLYGON
};

struct hyd_ent; // forward decl

/**
 * \struct hyd_coll_obj
 *
 * A collision object is a collection
 * of points that together form some
 * geometry that is tested for intersections
 */
struct hyd_coll_obj {
	struct hyd_ent *parent;
	struct hyd_v2 *points;
	uint32_t num_points;
	struct hyd_v2 center;
	float area;
	float radius;
	enum hyd_coll_type type;
	struct hyd_coll_obj *next;
};

/**
 * \struct hyd_coll
 *
 * Contains collision data for a collision
 */
struct hyd_coll {
	uint8_t intersects;
	uint8_t will_intersect;
	struct hyd_v2 minimum_translation_vector;
};

/**
 * \brief Creates a hyd_coll_obj from a JSON object
 *
 * \param[in] root The json object
 * \param[in] parent The parent entity
 *
 * \return The new collision_object
 */
struct hyd_coll_obj *hyd_coll_obj_create_json(json_t *root,
		struct hyd_ent *parent);

/**
 * \brief Creates a hyd_coll_obj list from a JSON array
 *
 * \param[out] list The list to fill
 * \param[in] root The json array
 * \param[in] parent The parent entity
 *
 * \return 0 on success, non-zero on error
 */
uint8_t hyd_coll_obj_create_json_arr(struct hyd_coll_obj *l, json_t *root,
		struct hyd_ent *parent);

/**
 * \param[in] col_obj The collision_object to destroy
 */
void hyd_coll_obj_destroy(struct hyd_coll_obj *col_obj);

/**
 * \brief Draws a collision object
 *
 * \param[in] col_obj The collision_object to draw
 * \param[in] renderer The renderer to use
 */
void hyd_coll_obj_draw(struct hyd_coll_obj *col_obj,
		struct SDL_Renderer *renderer);

/**
 * \param[in] col_obj The collision_object to calculate for
 *
 * \return The area of the collision_object
 */
float hyd_coll_obj_calc_area(struct hyd_coll_obj *col_obj);

float hyd_coll_obj_calc_radius(struct hyd_coll_obj *col_obj);

/**
 * Requires that the area of the hyd_coll_obj has been
 * calculated.
 *
 * \param[in] col_obj The hyd_coll_obj to calculate for
 *
 * \return The center of the hyd_coll_obj
 */
struct hyd_v2 hyd_coll_obj_calc_center(struct hyd_coll_obj *col_obj);

/**
 * \brief Project a hyd_coll_obj onto an axis, and return the min
 * and maximum points of the collision_object
 *
 * \param[in] col_obj The collision_object to project
 * \param[in] axis The axis to project onto
 * \param[out] min The minimum value on the axis
 * \param[out] max The maximum value on the axis
 */
void hyd_coll_obj_project(struct hyd_coll_obj *col_obj, struct hyd_v2 axis,
		float *min, float *max);

struct hyd_coll
*hyd_coll_check(struct hyd_coll_obj *obj1, struct hyd_coll_obj *obj2,
		float rel_x, float rel_y);

void hyd_coll_destroy(struct hyd_coll *coll);

#endif
