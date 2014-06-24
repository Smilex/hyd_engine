module hyd.list;

extern (C) {

struct hyd_list {
	hyd_list *prev;
	hyd_list *next;
}

}
