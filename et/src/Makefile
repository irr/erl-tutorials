.SUFFIXES: .erl .beam .hrl

APP = rbovespa
INCLUDE = ../include
OUTPUT = ../ebin
DOCDIR = ../edoc
MODULES = etrader etrader_app etrader_sup etrader_csv etrader_stats

all: compile

compile: $(OUTPUT) $(MODULES:%=$(OUTPUT)/%.beam)

edoc: $(DOCDIR) $(MODULES:%=$(DOCDIR)/%.html)

$(OUTPUT)/%.beam:%.erl
	erlc +debug_info -o $(OUTPUT) -I $(INCLUDE) -W $<

$(OUTPUT):
	mkdir $(OUTPUT)

$(DOCDIR)/%.html:%.erl
	erl -noshell -run edoc_run file '"$<"' '[{dir,"$(DOCDIR)"}]'

$(DOCDIR):
	erl -noshell -run edoc_run application $(APP) '"."' '[{dir,"$(DOCDIR)"}]'

clean:
	rm -rf $(OUTPUT)/*.beam $(DOCDIR) *~
