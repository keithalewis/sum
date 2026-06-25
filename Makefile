%.md: %.md.m4 macros.m4
	m4 macros.m4 $< > $@
