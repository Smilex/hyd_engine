module hyd.scene;
import hyd.layer;
import hyd.entity;

extern (C) nothrow {

struct hyd_scene {
	hyd_layer *layer_head;
}

hyd_ent **hyd_scene_find_ent_list(hyd_scene *s, const char *n, uint *num);

}
