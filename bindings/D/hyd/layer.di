module hyd.layer;
import hyd.entity;

extern (C) nothrow {

struct hyd_layer {
	hyd_ent *ent_head;
	hyd_layer *next;
	hyd_layer *prev;
}

}
