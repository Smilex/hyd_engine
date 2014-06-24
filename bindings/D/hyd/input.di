module hyd.input;

extern (C) nothrow {

uint hyd_input_preset_get_action_value(void *preset,
		const char *action);

uint hyd_input_get_max_value();
}
