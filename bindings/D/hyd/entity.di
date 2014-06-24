module hyd.entity;
import hyd.list;
import hyd.vector;

extern (C) nothrow {

struct hyd_ent {
	char[] name;
	hyd_v2 pos;
	hyd_list coll_objs;
}

hyd_ent *hyd_ent_list_find_first(hyd_list *l, const char *n);

float hyd_ent_get_number_property(hyd_ent *e, const char *n);

bool hyd_ent_get_bool_property(hyd_ent *e, const char *n);

string hyd_ent_get_string_property(hyd_ent *e, const char *n);

}
