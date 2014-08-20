module hyd.sprite;
import hyd.animation;

extern (C) nothrow {
struct hyd_spr {
	void *tex;
	void **frames;
	hyd_anim **anims;
	uint num_frames;
	uint num_anims;
}

}
