
TARGET = $(shell gcc -dumpmachine)

ifeq ($(TARGET), $(shell gcc -dumpmachine))
prefix = $(dir $(shell which gnatls))..
else
prefix = $(dir $(shell which $(TARGET)-gnatls))..
endif

BUILD_KIND=Debug
LIBRARY_TYPE=relocatable

PRJ_OPTS=-XLIBRARY_TYPE=$(LIBRARY_TYPE) -XPRJ_BUILD=$(BUILD_KIND)

all: build

build:
	gprbuild $(PRJ_OPTS) pragmarc.gpr

install:
	gprinstall -p --prefix=$(prefix) $(PRJ_OPTS) \
		--build-name=$(LIBRARY_TYPE) \
		pragmarc.gpr

clean:
	gprclean $(PRJ_OPTS) pragmarc.gpr
