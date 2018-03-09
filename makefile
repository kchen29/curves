sbcl := sbcl --noinform --non-interactive --load "load.lisp" --eval
script := "script"

all:
	$(sbcl) '(main $(script))'

test:
	$(sbcl) '(main-test)'

clean:
	rm -f *~ *.fasl *.ppm *.png
