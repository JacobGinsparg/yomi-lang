main: linux_uinput.o
	gcc -shared -o linux_uinput.so linux_uinput.o

linux_uinput.o: linux_uinput.c
	gcc -c -fPIC -o linux_uinput.o linux_uinput.c