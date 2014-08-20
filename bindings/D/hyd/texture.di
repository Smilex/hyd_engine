module hyd.texture;
import hyd.geom;

extern (C) nothrow {

struct hyd_tex {
	void *ptr;
	uint *ref_count;
	char *name;
	SDL_Rect size;
	uint format;
}

hyd_tex *hyd_tex_create_file(const char *fname);

void hyd_tex_destroy(hyd_tex *texture);

}
