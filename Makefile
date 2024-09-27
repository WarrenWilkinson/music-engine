# Okay, at least two libraries...
# one is wireplumber AND asound for a control midi interface.

.PHONY : all clean
all: build/libbuttonloopback.so build/libmusicengine.so

# The music engine is a shim to control wireplumber via LISP
build/libmusicengine.so: src/c/musicengine/alsa.c src/c/musicengine/alsa.h src/c/musicengine/wireplumber.c src/c/musicengine/wireplumber.h src/c/musicengine/musicengine.c 
	gcc -Wall -shared -o $@ $^ `pkg-config --libs --cflags gobject-2.0 --cflags wireplumber-0.4 --cflags alsa`

build/libbuttonloopback.so: src/c/buttonloopback/buttonloopback.c
	gcc -Wall -shared -o $@ $^

# The simulator is a GTK application that pretends to be the hardware
build/libsimulator.so: src/c/simulator/simulator.c
	gcc -Wall -shared -o $@ $^ `pkg-config --libs --cflags gtk4 --cflags cairo`

clean:
	rm -f build/musicengine.so build/simulator.so
