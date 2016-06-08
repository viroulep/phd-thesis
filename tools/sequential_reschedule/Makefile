
all: libtool_schedule.so libgomp.so libgomp.so.1

%.o: %.c
	gcc -c -fpic -o $@ $<
libtool_schedule.so: reschedule.o
	gcc -shared -o libtool_schedule.so reschedule.o -Wl,--version-script,libgomp.map

libgomp.so:
	ln -s libtool_schedule.so libgomp.so

libgomp.so.1:
	ln -s libtool_schedule.so libgomp.so.1

clean:
	rm -rf libtool_schedule.so reschedule.o libgomp.so libgomp.so.1
