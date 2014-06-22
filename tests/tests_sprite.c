#include "tests.h"
#include "sprite.h"

START_TEST (test_sprite)
{
	struct hyd_spr *spr = hyd_spr_create(NULL, NULL, 0, NULL, 0);

	ck_assert_int_eq(spr->num_frames, 0);
	ck_assert_int_eq(spr->num_anims, 0);
	ck_assert_ptr_eq(spr->tex, NULL);
	ck_assert_ptr_eq(spr->frames, NULL);
	ck_assert_ptr_eq(spr->anims, NULL);

	hyd_spr_destroy(spr);
}
END_TEST

Suite *sprite_suite(void)
{
	Suite *s = suite_create("Sprite");

	TCase *tc_core = tcase_create("Core");
	tcase_add_test(tc_core, test_sprite);
	suite_add_tcase(s, tc_core);

	return s;
}
