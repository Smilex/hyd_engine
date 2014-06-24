/** \file input.h
 * Contains structs and functions for inputs
 */

#ifndef HYD_INPUT_H
#define HYD_INPUT_H

#include <SDL.h>
#include <stdint.h>
#include <jansson.h>
#include "list.h"

/**
 * \enum hyd_input_type
 *
 * Defines the type of the input
 */
enum hyd_input_type {
	KEY
};

typedef void (*hyd_input_callback)(void*, const char*);

/**
 * \struct hyd_input
 *
 * A user input is a association of
 * an action and an input
 */
struct hyd_input {
	char *action;
	uint16_t value;
	enum hyd_input_type type;
	uint8_t code;
	hyd_input_callback callback;
	struct hyd_list list;
};

/**
 * \struct hyd_input_preset
 *
 * An input preset is a collection of
 * inputs associated with a name
 */
struct hyd_input_preset {
	char *name;
	struct hyd_list inputs;
	struct hyd_list list;
};

/**
 * \brief Creates an input as a keyboard key
 *
 * \param[in] action Name of the action
 * \param[in] code The key code
 *
 * \return The new input. NULL if error.
 */
struct hyd_input *hyd_input_create_key(const char *action, uint8_t code);

/**
 * \brief Creates an input from a json object
 *
 * \param[in] root The json object
 *
 * \return The new input
 */
struct hyd_input *hyd_input_create_json(const char *action, json_t *root);

/**
 * \param[in] name The name of the preset
 *
 * \return The new input preset. NULL if error.
 */
struct hyd_input_preset *hyd_input_preset_create(const char *name);

/**
 * \brief Creates input presets from file
 *
 * \param[out] list The list to append to
 * \param[in] filename Path to the file to read
 *
 * \return 0 on success. Non-zero on error
 */
uint8_t hyd_input_preset_list_create_file(struct hyd_list *list, const char *filename);

/**
 * \brief Creates a input preset from a JSON object
 *
 * \param[in] name Name of the preset
 * \param[in] root The JSON object
 *
 * \return The new input preset
 */
struct hyd_input_preset *hyd_input_preset_create_json(const char *name, json_t *root);

/**
 * \brief Get the value of the input associated with the action
 *
 * \param[in] preset The input preset to look through
 * \param[in] action The action to look for
 *
 * \return The value or 0 if not found.
 */
uint16_t hyd_input_preset_get_action_value(struct hyd_input_preset *preset,
		const char *action);

/**
 * \brief Adds a callback to an action
 *
 * \param[in] preset The input preset
 * \param[in] action The input action
 * \param[in] callback The callback
 */
void hyd_input_preset_add_callback(struct hyd_input_preset *preset, const char *action,
		hyd_input_callback callback);

/**
 * \param[in] input The input to destroy
 */
void hyd_input_destroy(struct hyd_input *input);

/**
 * \param[in] preset The input preset to destroy
 */
void hyd_input_preset_destroy(struct hyd_input_preset *preset);

/**
 * \brief Get the maximum possible value for any input
 *
 * \return The maximum value
 */
uint16_t hyd_input_get_max_value(void);

#endif // OPENHYDORAH_INPUT_H
