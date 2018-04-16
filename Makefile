lib/linux_uinput.so: lib/linux_uinput.o
	gcc -shared -o lib/linux_uinput.so lib/linux_uinput.o

lib/linux_uinput.o: lib/linux_uinput.c
	gcc -c -fPIC -o lib/linux_uinput.o lib/linux_uinput.c

uinput: lib/linux_uinput.so
	sudo modprobe uinput
	sudo chmod +0666 /dev/uinput

drracket:
	@ LD_LIBRARY_PATH=lib nohup drracket > /dev/null 2>&1 &

test:
	raco test test

.PHONY: drracket test
