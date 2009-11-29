C_SRC = c_src
DOC = doc
EBIN = ebin
PRIV = priv
SRC = src
PWD = $(shell pwd)

ERL = erl
ERLC = erlc
CFLAGS = -pa $(EBIN) -o $(EBIN)

APP = decimal
ERL_LIB = $(shell erl -eval 'io:format("~s~n", [code:lib_dir()])' \
            -s init stop -noshell | grep '/lib')
VSN = $(shell erl -pa `pwd`/ebin -noshell -eval \
         'decimal:start(),{ok,Rev}=application:get_key(decimal,vsn),io:format("~s~n",[Rev])' \
          -s erlang halt | tail -n 1)
INSTALL_DIR =  $(ERL_LIB)/$(APP)-$(VSN)


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
	@erl -noinput -eval \
             'edoc:application($(APP), "./", [{doc, "doc/"}, {files, "src/"}])' \
             -s erlang halt

make_boot:
	@(cd $(EBIN); $(ERL) -pa $(EBIN) -noshell \
              -run make_boot write_scripts rest_app)

start: all
	@$(ERL) -pa $(PWD)/$(EBIN) -run $(APP)

test: all
	@$(ERL) -pa $(PWD)/$(EBIN) -noshell \
              -eval 'eunit:test(decimal,[verbose])' -s erlang halt

install:
	@echo Installing decimal-$(VSN)
	@echo Installing to $(INSTALL_DIR)
	if [ -d '$(INSTALL_DIR)' ];then rm -fr $(INSTALL_DIR); fi
	mkdir $(INSTALL_DIR)
	cp -r $(EBIN) $(INSTALL_DIR)
	cp -r $(PRIV) $(INSTALL_DIR)
	cp -r $(SRC) $(INSTALL_DIR)


uninstall: 
	if [ -d '$(INSTALL_DIR)' ];then rm -fr $(INSTALL_DIR); fi


clean:
	rm -rf $(EBIN)/*.beam erl_crash.dump
	rm -rf $(EBIN)/*.boot $(EBIN)/*.rel $(EBIN)/*.script
	rm -rf $(DOC)/*.html $(DOC)/*.css $(DOC)/erlang.png doc/edoc-info
	$(MAKE) --directory=$(C_SRC) clean

