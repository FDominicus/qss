# Makefile for qss-odbc-module, the ODBC module of qss.
# SPDX-License-Identifier: MIT
#
#   make                        Linux/BSD with unixODBC (package unixodbc-dev)
#   make EMACS_INCLUDE=~/src/emacs/src   emacs-module.h from a source tree
#   make ODBC=iodbc             macOS/BSD with iODBC
#   make OS=Windows_NT          MinGW-w64 (MSYS2), links odbc32
#
# Usually run from Emacs with M-x qss-odbc-build-module.

EMACS_INCLUDE ?=
CC     ?= cc
CFLAGS ?= -O2 -g -Wall -Wextra
SRC     = qss-odbc-module.c

ifeq ($(OS),Windows_NT)
  TARGET = qss-odbc-module.dll
  LIBS   = -lodbc32
  PIC    =
else
  UNAME := $(shell uname -s)
  ifeq ($(UNAME),Darwin)
    TARGET = qss-odbc-module.dylib
  else
    TARGET = qss-odbc-module.so
  endif
  PIC = -fPIC
  ifeq ($(ODBC),iodbc)
    LIBS = -liodbc
  else
    LIBS = -lodbc
  endif
endif

INCLUDES = $(if $(EMACS_INCLUDE),-I$(EMACS_INCLUDE),)

all: $(TARGET)

$(TARGET): $(SRC)
	$(CC) $(CFLAGS) $(PIC) $(INCLUDES) -shared -o $@ $(SRC) $(LIBS)

clean:
	rm -f qss-odbc-module.so qss-odbc-module.dylib qss-odbc-module.dll

.PHONY: all clean
