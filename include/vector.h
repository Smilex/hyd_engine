/**
 * \file vector.h
 */

#ifndef HYD_VECTOR_H
#define HYD_VECTOR_H

/**
 * \struct hyd_v2
 *
 * A 2-dimensional vector
 */
struct hyd_v2 {
	float x;
	float y;
};

/**
 * \brief Returns the Dot product of two 2D vectors
 *
 * \param[in] lhs Left-hand side vector
 * \param[in] rhs Right-hand side vector
 *
 * \return The dot product
 */
float hyd_v2_dot_product(struct hyd_v2 lhs, struct hyd_v2 rhs);

/**
 * \brief Returns the normalized 2D vector
 *
 * \param[in] vec The vector to normalize
 *
 * \return The normalized version of vec
 */
struct hyd_v2 hyd_v2_normalize(struct hyd_v2 vec);

float hyd_v2_length(struct hyd_v2 vec);

struct hyd_v2 hyd_v2_substract(struct hyd_v2 lhs, struct hyd_v2 rhs);

struct hyd_v2 hyd_v2_add(struct hyd_v2 lhs, struct hyd_v2 rhs);

#endif
