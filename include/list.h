/**
 * \file list.h
 *
 * This has been heavily inspired by
 * http://kernelnewbies.org/FAQ/LinkedLists
 * and
 * http://www.makelinux.net/ldd3/chp-11-sect-5
 */

#ifndef HYD_LIST_H
#define HYD_LIST_H

struct hyd_list {
	struct hyd_list *prev;
	struct hyd_list *next;
};

/**
 * \param[in] list The list to initialize
 *
 */
void hyd_list_init(struct hyd_list *list);

/**
 * \param[in] new The list to add to head
 * \param[in] head The list to append to
 *
 */
void hyd_list_append(struct hyd_list *n, struct hyd_list *h);

/**
 * \brief Links the prev and next of the list together
 *
 * \param[in] list The list to remove
 */
void hyd_list_remove(struct hyd_list *list);

/**
 * \brief Iterate over a list
 *
 * \param[in] iter The &struct list iterator
 * \param[in] head The list to iterate
 */
#define hyd_list_for_each(iter, head) \
	for (iter = (head)->next; iter != (head); iter = iter->next)

/**
 * \brief Iterate over a list safely, so that entries can be removed
 *
 * \param[in] iter The &struct list iterator
 * \param[in] n The &struct list variable to use for temporary storage
 * \param[in] head The list to iterate
 */
#define hyd_list_for_each_safe(iter, n, head) \
	for (iter = (head)->next, n = iter->next; iter != (head); \
			iter = n, n = iter->next)

/**
 * \brief Get the struct for this entry
 *
 * \param[in] ptr The &struct list iterator
 * \param[in] type The type of the entry
 * \param[in] member The name of the list member of the entry
 *
 * \return Pointer to the entry
 */
#define hyd_list_entry(ptr, type, member) ({ \
		const typeof(((type *) 0)->member) *__mptr = (ptr); \
		(type *)((char *)__mptr - ((size_t) &((type *) 0)->member)); })

/**
 * \brief Iterate over the entries of a list
 *
 * \param[in] iter The iterator as a type * of the entries
 * \param[in] head The list to iterate over
 * \param[in] member The name of the list member of the entry
 *
 */
#define hyd_list_for_each_entry(iter, head, member) \
	for (iter = hyd_list_entry((head)->next, typeof(*iter), member); \
			&iter->member != (head); \
			iter = hyd_list_entry(iter->member.next, typeof(*iter), member))

/**
 * \brief Iterate over the entries of a list safely, so that entries
 * can be removed
 *
 * \param[in] iter The iterator as a type * of the entries
 * \param[in] n The type * variable to use for temporary storage
 * \param[in] head The list to iterate
 * \param[in] member The name of the list member of the entry
 */
#define hyd_list_for_each_entry_safe(iter, n, head, member) \
	for (iter = hyd_list_entry((head)->next, typeof(*iter), member), \
			n = hyd_list_entry(iter->member.next, typeof(*iter), member); \
			&iter->member != (head); \
			iter = n, n = hyd_list_entry(iter->member.next, typeof(*iter), member)) \

#endif
