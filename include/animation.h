/**
 * \file animation.h
 */

#ifndef HYD_ANIMATION_H
#define HYD_ANIMATION_H

#include "frame.h"
#include <stdint.h>

struct hyd_anim {
	char *name;
	struct hyd_frame **frames;
	uint32_t num_frames;
};

/**
 * \param[in] name The name of the animation
 * \param[in] frames The array of frames
 * \param[in] num_frames The number of frames
 *
 * \return The new animation
 */
struct hyd_anim *hyd_anim_create(const char *name, struct hyd_frame **frames,
		uint32_t num_frames);

/**
 * \param[in] root The JSON object
 * \param[in] frames The frames to do look-up from
 * \param[in] num_frames The number of frames
 *
 * \return The new animation
 */
struct hyd_anim *hyd_anim_create_json(json_t *root, struct hyd_frame **frames,
		uint32_t num_frames);

/**
 * \param[in] root The JSON array
 * \param[in] frames The frames to do look-up from
 * \param[in] num_frames The number of frames
 * \param[out] num The number of animations created
 *
 * \return The new array of animations
 */
struct hyd_anim **hyd_anim_array_create_json(json_t *root, struct hyd_frame **frames,
		uint32_t num_frames, uint32_t *num);

/**
 * \param[in] anim The animation to destroy
 */
void hyd_anim_destroy(struct hyd_anim *anim);

#endif
