
_build/install/default/bin/pelzl:
	dune build

_build/install/default/bin/pelzl-curses-keys:
	dune build

install: _build/install/default/bin/pelzl _build/install/default/bin/pelzl-curses-keys
	ORPIE_PREFIX=`scripts/compute_prefix eval` && \
	mkdir -p "$(DESTDIR)/$$ORPIE_PREFIX" && \
	dune install --prefix="$(DESTDIR)/$$ORPIE_PREFIX"

clean:
	dune clean

.PHONY: install clean

