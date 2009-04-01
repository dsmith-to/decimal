.PHONY: clean
.SUFFIXES: .o .c .cc

OS= ${shell uname}
CC=gcc
CFLAGS=-fPIC -O2
LIBTOOL=libtool -dynamic -undefined suppress -flat_namespace
LD=ld -bundle -undefined suppress -flat_namespace

LIBS        = -lc -lgcc
# Erlang
ERL_INCLUDE = -I/usr/local/lib/otp/erlang/usr/include 
ERL_LIBS    = -L/usr/local/lib/otp/erlang/usr/lib \
              -lerts
EI_INCLUDE  = -I/usr/local/lib/otp/erlang/lib/erl_interface-3.5.8/include
EI_LIBS     = -L/usr/local/lib/otp/erlang/lib/erl_interface-3.5.8/lib \
              -lei \
              -lerl_interface

TARGET_LIB = decimal_drv.so
ifeq ($(OS), Darwin)
  EXTRA_OPTIONS = -fno-common -bundle -undefined suppress -flat_namespace
endif
ALL: $(TARGET_LIB)

.c.o:
	$(CC) $(CFLAGS) -Wall $(ERL_INCLUDE) $(EI_INCLUDE) -c $<

decimal_drv.so: decContext.o decNumber.o decimal64.o decimal128.o decQuad.o decPacked.o decimal_drv.o
	$(LD) -o $@ $(LIBS) $(ERL_LIBS) $(EI_LIBS)  $^
	mv decimal_drv.so ../priv/lib 

clean:
	rm -f *.o *.so
