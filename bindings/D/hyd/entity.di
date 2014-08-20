module hyd.entity;
import hyd.vector;
import hyd.sprite;
import hyd.collision;
import hyd.layer;
import hyd.quad;

extern (C) nothrow {

enum HYD_ENT_COLL {NONE = 0x0, LEFT = 0x1, RIGHT = 0x2, TOP = 0x4, BOTTOM = 0x8};

struct hyd_ent {
	hyd_spr *spr;
	char *name;
	hyd_v2 pos;
	hyd_ent *parent;

	hyd_ent *next;
	hyd_ent *prev;
	hyd_ent *children;
	void *properties;
	hyd_quad coll;
	hyd_layer *layer;
}

hyd_ent *hyd_ent_create_file(const char *fname,
		hyd_ent *parent, hyd_layer *layer);


hyd_ent **hyd_ent_list_find(hyd_ent *l, const char *n, uint *num);

float hyd_ent_get_number_property(hyd_ent *e, const char *n);

bool hyd_ent_get_bool_property(hyd_ent *e, const char *n);

string hyd_ent_get_string_property(hyd_ent *e, const char *n);

float hyd_ent_set_number_property(hyd_ent *e, float value, const char *n);

void hyd_ent_draw(hyd_ent *ent);

void hyd_ent_destroy(hyd_ent *e);

hyd_ent *hyd_ent_copy(hyd_ent *e);

HYD_ENT_COLL hyd_ent_coll(hyd_ent *l, hyd_ent *r);

hyd_v2 hyd_ent_get_pos(hyd_ent *ent);
}
