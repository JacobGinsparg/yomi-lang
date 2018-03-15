main: lib/linux_uinput.o
	gcc -shared -o lib/linux_uinput.so lib/linux_uinput.o

lib/linux_uinput.o: lib/linux_uinput.c
	gcc -c -fPIC -o lib/linux_uinput.o lib/linux_uinput.c
