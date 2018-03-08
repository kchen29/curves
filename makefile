main := (main)
sbcl := sbcl --noinform --non-interactive

all:
	$(sbcl) --load "load.lisp" --eval '(progn $(main))'

circles:
	$(sbcl) --load "load.lisp" --load "script.lisp" --eval '(main-script)'

clean:
	rm -f *~ *.fasl *.ppm *.png
