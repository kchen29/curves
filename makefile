sbcl := sbcl --noinform --non-interactive --load "load.lisp" --eval
script := "script"

all:
	$(sbcl) '(main $(script))'

test:
	$(sbcl) '(main-test)'

image:
	$(sbcl) '(main "shrug")'

clean:
	rm -f *~ *.fasl *.ppm *.png
