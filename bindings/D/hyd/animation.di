module hyd.animation;

extern (C) nothrow {
struct hyd_anim {
	char *name;
	void **frames;
	uint num_frames;
	uint curr_frame;
	ubyte repeat;
	uint delay;
	uint last_time;
	uint start_frame;
}

void *hyd_anim_get_next(hyd_anim *anim);

void *hyd_anim_get_prev(hyd_anim *anim);
}
