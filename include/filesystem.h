/**
 * \file filesystem.h
 */

#ifndef HYD_FILESYSTEM_H
#define HYD_FILESYSTEM_H

#include <physfs.h>
#include <stdint.h>

/**
 * \brief Reads entire file and returns the memory buffer
 *
 * \param[in] filename Path to the file to read
 * \param[out] buf The buffer to fill with data
 *
 * \return Amount of bytes read. If 0, then \em buf is invalid
 */
PHYSFS_sint64 hyd_fs_read_buffer(const char *filename, uint8_t **buf);

int hyd_fs_add_path(const char *path, const char *as);

#endif
