#include "tests.h"
#include "entity.h"

START_TEST (test_entity)
{
	struct hyd_ent *ent = hyd_ent_create(NULL, "ent", NULL);
	struct hyd_ent *child = hyd_ent_create(NULL, "child", ent);

	ck_assert_str_eq(ent->name, "ent");
	ck_assert_str_eq(child->name, "child");
	ck_assert_ptr_eq(ent->parent, NULL);
	ck_assert_ptr_eq(child->parent, ent);

	hyd_ent_destroy(ent); // destroys 'child'
}
END_TEST

Suite *entity_suite(void)
{
	Suite *s = suite_create("Entity");

	TCase *tc_core = tcase_create("Core");
	tcase_add_test(tc_core, test_entity);
	suite_add_tcase(s, tc_core);

	return s;
}
