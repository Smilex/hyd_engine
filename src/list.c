#include "list.h"

void hyd_list_init(struct hyd_list *list)
{
	list->next = list;
	list->prev = list;
}

void hyd_list_append(struct hyd_list *n, struct hyd_list *h)
{
	struct hyd_list *iter = h;

	while (iter->next != h)
	{
		iter = iter->next;
	}

	iter->next = n;
	n->next = h;
	n->prev = iter;
}

void hyd_list_remove(struct hyd_list *list)
{
	list->prev->next = list->next;
	list->next->prev = list->prev;
}
