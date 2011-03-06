OUTDIR = ../ebin
SOURCES = $(wildcard *.erl) sql92_parser.erl sql92_scan.erl
BEAMS = $(addprefix $(OUTDIR)/,$(SOURCES:.erl=.beam))

all:    $(BEAMS)

$(OUTDIR)/%.beam: %.erl
	erlc -W $(EFLAGS) -o $(OUTDIR) $<

sql92_parser.erl: sql92_parser.yrl
	erlc -W $<

sql92_scan.erl: sql92_scan.xrl
	erlc -W $<

clean:
	-rm -f $(BEAMS) sql92_parser.erl sql92_scan.erl