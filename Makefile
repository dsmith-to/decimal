APP = decimal

C_SRC = c_src
DOC = doc
EBIN = ebin
SRC = src
PWD = $(shell pwd)

ERL = erl
ERLC = erlc
CFLAGS = -pa $(EBIN) -o $(EBIN)

.PHONY: c_src

all: c_src ebin compile
all_boot: all make_boot

c_src:
	$(MAKE) --directory=$(C_SRC)

compile:
	@echo Compiling $(APP) from srcs
	$(ERLC) $(CFLAGS) $(SRC)/*.erl

edoc:
	@echo Generating $(APP) documentation from srcs
	erl -noinput -eval \
            'edoc:application($(APP), "./", [{doc, "doc/"}, {files, "src/"}])' \
            -s erlang halt

make_boot:
	(cd $(EBIN); $(ERL) -pa $(EBIN) -noshell \
              -run make_boot write_scripts rest_app)

start: all
	$(ERL) -pa $(PWD)/$(EBIN) -run $(APP)

test: all
	$(ERL) -pa $(PWD)/$(EBIN) -noshell \
              -eval 'eunit:test(decimal,[verbose])' -s erlang halt

clean:
	rm -rf $(EBIN)/*.beam erl_crash.dump
	rm -rf $(EBIN)/*.boot $(EBIN)/*.rel $(EBIN)/*.script
	rm -rf $(DOC)/*.html $(DOC)/*.css $(DOC)/erlang.png doc/edoc-info
	$(MAKE) --directory=$(C_SRC) clean

