lib/linux_uinput.so: lib/linux_uinput.o
	gcc -shared -o lib/linux_uinput.so lib/linux_uinput.o

lib/linux_uinput.o: lib/linux_uinput.c
	gcc -c -fPIC -o lib/linux_uinput.o lib/linux_uinput.c

drracket:
	@ LD_LIBRARY_PATH=lib nohup drracket > /dev/null 2>&1 &

test:
	raco test test

.PHONY: drracket test
