module hyd.collision;

extern (C) nothrow {

struct hyd_coll_obj {
}

struct hyd_coll {
	bool intersects;
	bool will_intersect;
}

hyd_coll
*hyd_coll_list_check(void *list, hyd_coll_obj *obj,
		float rel_x, float rel_y);

hyd_coll
*hyd_coll_check(hyd_coll_obj *obj1, hyd_coll_obj *obj2,
		float rel_x, float rel_y);

}
