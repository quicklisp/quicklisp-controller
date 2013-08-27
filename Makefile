all: system-file-magic depcheck

depcheck: asdf.lisp depcheck.lisp
	buildapp --dynamic-space-size 4000 --output depcheck --load asdf.lisp --require sb-aclrepl  --require sb-bsd-sockets --require sb-rt --require sb-cover --require sb-sprof --entry depcheck:main --require sb-introspect --require sb-cltl2 --require sb-posix --require sb-concurrency --load depcheck.lisp


system-file-magic: asdf.lisp system-file-magic.lisp
	buildapp --dynamic-space-size 4000 --output system-file-magic --load asdf.lisp --require sb-posix --load system-file-magic.lisp --entry system-file-magic:main

install: system-file-magic depcheck
	install -c -m 555 system-file-magic $(HOME)/bin
	install -c -m 555 depcheck $(HOME)/bin

clean:
	rm -f system-file-magic depcheck
