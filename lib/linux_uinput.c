/*
Extremely minimal functions to expose uinput features to Yomi through Racket's
FFI. The intention is to avoid writing C code as much as possible. Instead,
write basic building blocks in C that we can use in Racket to build more
sophisticated functions.
*/

#include <linux/uinput.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>

int setup_uinput_device()
{
	struct uinput_setup usetup;

	int fd = open("/dev/uinput", O_WRONLY | O_NONBLOCK);
	if (fd < 0) {
		return -1;
	}

	ioctl(fd, UI_SET_EVBIT, EV_KEY);
	// Event codes [16, 25]
	ioctl(fd, UI_SET_KEYBIT, KEY_Q);
	ioctl(fd, UI_SET_KEYBIT, KEY_W);
	ioctl(fd, UI_SET_KEYBIT, KEY_E);
	ioctl(fd, UI_SET_KEYBIT, KEY_R);
	ioctl(fd, UI_SET_KEYBIT, KEY_T);
	ioctl(fd, UI_SET_KEYBIT, KEY_Y);
	ioctl(fd, UI_SET_KEYBIT, KEY_U);
	ioctl(fd, UI_SET_KEYBIT, KEY_I);
	ioctl(fd, UI_SET_KEYBIT, KEY_O);
	ioctl(fd, UI_SET_KEYBIT, KEY_P);
	// Event codes 103, 105, 106, 107
	ioctl(fd, UI_SET_KEYBIT, KEY_UP);
	ioctl(fd, UI_SET_KEYBIT, KEY_LEFT);
	ioctl(fd, UI_SET_KEYBIT, KEY_RIGHT);
	ioctl(fd, UI_SET_KEYBIT, KEY_DOWN);

	memset(&usetup, 0, sizeof(usetup));
	usetup.id.bustype = BUS_USB;
	//usetup.id.vendor = 0x1234; /* sample vendor */
	//usetup.id.product = 0x5678; /* sample product */
	strcpy(usetup.name, "Yomi virtual keyboard");

	ioctl(fd, UI_DEV_SETUP, &usetup);
	ioctl(fd, UI_DEV_CREATE);

	return fd;
}

void teardown_uinput_device(int fd)
{
    ioctl(fd, UI_DEV_DESTROY);
    close(fd);
}

void emit(int fd, int type, int code, int val)
{
   struct input_event ie;

   ie.type = type;
   ie.code = code;
   ie.value = val;

   write(fd, &ie, sizeof(ie));
}