SOURCES = \
Draw.ml \
Interface.ml \
Game.ml 

all: $(SOURCES)
	corebuild -quiet -lib graphics Main.native

check: $(SOURCES)
	@chmod u+x ../check_width
	@../check_width Draw.ml; \
	../check_width Interface.ml; \
	../check_width Game.ml

clean:
	rm -rf _build Main.native
