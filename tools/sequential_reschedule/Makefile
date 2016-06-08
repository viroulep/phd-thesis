
all: libtool_schedule.so

%.o: %.c
	gcc -c -fpic -o $@ $<
libtool_schedule.so: reschedule.o
	gcc -shared -o libtool_schedule.so reschedule.o -Wl,--version-script,libgomp.map
	ln -s libtool_schedule.so libgomp.so
	ln -s libtool_schedule.so libgomp.so.1

clean:
	rm -rf libtool_schedule.so reschedule.o libgomp.so libgomp.so.1
