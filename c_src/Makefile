.PHONY: clean
.SUFFIXES: .o .c .cc

ERLI_LIB = $(shell erl -eval 'io:format("~s~n", [code:lib_dir(erl_interface)])' \
             -s init stop -noshell | grep '/lib')
ERL_LIB = $(shell erl -eval 'io:format("~s~n", [code:lib_dir()])' \
            -s init stop -noshell | grep '/lib')

PLATFORM = $(shell uname -sm | sed 's/ /-/g')
include $(PLATFORM).mk

LIBS        = -lc -lgcc
# Erlang
ERL_INCLUDE = -I$(ERL_LIB)/../usr/include 
ERL_LIBS    = -L$(ERL_LIB)/../usr/lib -lerts
EI_INCLUDE  = -I$(ERLI_LIB)/include
EI_LIBS     = -L$(ERLI_LIB)/lib -lei -lerl_interface

TARGET_LIB = decimal_drv.so
OBJ = decContext.o decNumber.o decimal64.o decimal128.o \
      decQuad.o decPacked.o decimal_drv.o

all: $(TARGET_LIB)

.c.o:
	$(CC) $(CFLAGS) -Wall $(ERL_INCLUDE) $(EI_INCLUDE) -c $<

$(TARGET_LIB): $(OBJ)
	$(LD) $(LIBS) $(ERL_LIBS) $(EI_LIBS) -o $@  $^
	cp decimal_drv.so ../priv/lib 

clean:
	rm -f *.o *.so
