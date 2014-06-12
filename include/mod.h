/**
 * \file mod.h
 */

#ifndef HYD_MOD_H
#define HYD_MOD_H

#include <stdint.h>

/**
 * \struct hyd_mod_info
 * \brief Contains information for a mod
 */
struct hyd_mod_info {
	char *name;
	char *description;
	char *filename;
	void *handle;
};

/**
 * \struct hyd_mod
 *
 * A mod is external code that
 * acts on specific calls
 * required by the engine
 */
struct hyd_mod {
	struct hyd_mod_info *info;
	void (*update)(void*, uint32_t);
	void (*init)(void*);
	void (*destroy)(void*);
};

/**
 * \brief Queries a mod for information
 *
 * Calls a function in the mod that is expected
 * to return a specfic amount of information,
 * required to fill mod_info.
 *
 * \param[in] filename Path to the mod
 *
 * \return The new mod_info or NULL on error.
 */
struct hyd_mod_info *hyd_mod_info_get(const char *filename);

/**
 * \param[in] info The mod_info to destroy
 */
void hyd_mod_info_destroy(struct hyd_mod_info *info);

/**
 * Creates the mod and informs the mod of initilization
 *
 * \param[in] info The mod's information
 *
 * \return The new Mod
 */
struct hyd_mod *hyd_mod_create(struct hyd_mod_info *info);

/**
 * Destroys the mod, along with the ModInfo.
 * Also informs the mod of exit
 *
 * \param[in] mod The Mod to destroy
 */
void hyd_mod_destroy(struct hyd_mod *mod);

/**
 * Calls the mod's update function
 *
 * \param[in] mod The mod to update
 * \param[in] dt The delta time since last called
 */
void hyd_mod_update(struct hyd_mod *mod, uint32_t dt);

#endif
