UNAME_S := $(shell uname -s)

##---------------------------------------------------------------------
## BUILD FLAGS PER PLATFORM
##---------------------------------------------------------------------

ifeq ($(UNAME_S), Linux) #LINUX
	ECHO_MESSAGE = "Linux"
	LDFLAGS += $(shell sdl2-config --libs)

	CXXFLAGS += $(shell sdl2-config --cflags)
	CFLAGS = $(CXXFLAGS)
endif

ifeq ($(UNAME_S), Darwin) #APPLE
	ECHO_MESSAGE = "Mac OS X"
	LDFLAGS += $(shell sdl2-config --libs)
	LDFLAGS += -L/usr/local/lib -L/opt/local/lib

	CXXFLAGS += $(shell sdl2-config --cflags)
	CXXFLAGS += -I/usr/local/include -I/opt/local/include
	CFLAGS = $(CXXFLAGS)
endif

ifeq ($(OS), Windows_NT)
	ECHO_MESSAGE = "MinGW"
	LDFLAGS += $(shell pkg-config --static --libs sdl2) -lchibi-scheme

	CXXFLAGS += $(shell pkg-config --cflags sdl2)
	CFLAGS = $(CXXFLAGS)
endif

LDFLAGS += -lchibi-scheme

all: main

clean:
	rm -f *.o main

main: main.o
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS) 
main.o: main.c
	$(CC) $(CFLAGS) -c $< -o $@ 

