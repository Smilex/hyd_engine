module hyd.input;

extern (C) nothrow {

struct hyd_input {
}

struct hyd_ip {
	char[] name;
	uint count;
	hyd_input[] inputs;
	hyd_ip *next;
	hyd_ip *prev;
}

uint hyd_ip_get_value(hyd_ip *preset, const char *action);

uint hyd_input_get_max_value();
}
