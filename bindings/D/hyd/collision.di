module hyd.collision;
import hyd.list;

extern (C) nothrow {

struct hyd_coll_obj {
}

struct hyd_coll {
	bool intersects;
	bool will_intersect;
}

hyd_coll
*hyd_coll_list_check(hyd_list *list, hyd_coll_obj *obj,
		float rel_x, float rel_y);

hyd_coll
*hyd_coll_list_check_list(hyd_list *list1, hyd_list *list2,
		float rel_x, float rel_y);

}
