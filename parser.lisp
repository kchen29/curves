(defmacro switch (value test &body cases)
  "Macro for switch-case statements.
   TESTs VALUE with the first element in each case of CASES.
   If otherwise is the first element, then it acts as the default case."
  `(cond
     ,@(loop for case in cases
             for test-value = (first case)
             for return-value = (rest case)
             if (eql 'otherwise test-value)
               collect `(t ,@return-value)
             else
               collect `((funcall ,test ,value ,test-value) ,@return-value))))

(defun parse-file (filename edges transform dimensions screen)
  "Parses FILENAME. Uses EDGES and TRANSFORM matrices to store edges
   and the transform matrix. Commands write to SCREEN.
   The file follows the following format:
     Every command is a single string that takes up a line
     Any command that requires arguments must have those arguments in the second line.
     The commands are as follows:
         line: add a line to the edge matrix -
	    takes 6 arguments (x0 y0 z0 x1 y1 z1)
	 ident: set the transform matrix to the identity matrix -
	 scale: create a scale matrix,
	    then multiply the transform matrix by the scale matrix -
	    takes 3 arguments (sx sy sz)
	 move: create a translation matrix,
	    then multiply the transform matrix by the translation matrix -
	    takes 3 arguments (tx ty tz)
	 rotate: create a rotation matrix,
	    then multiply the transform matrix by the rotation matrix -
	    takes 2 arguments (axis theta) axis should be x, y or z.
            Theta is in degrees
	 apply: apply the current transformation matrix to the edge matrix
	 display: draw the lines of the edge matrix to the screen, then display the screen
	 save: draw the lines of the edge matrix to the screen
	    save the screen to a file -
	    takes 1 argument (filename)
	 quit: end parsing."
  (with-open-file (stream filename)
    (do ((line (next-line stream) (next-line stream)))
        ((string= line "quit"))
      (parse-line line stream edges transform dimensions screen))))

(defun parse-line (line stream edges transform dimensions screen)
  "Parses line according to parse-file."
  (if (command-no-args line)
      (switch line #'string=
        ("ident" (to-identity transform))
        ("apply" (matrix-multiply transform edges))
        ("display" (draw-lines edges screen '(255 0 255))
                   (display dimensions screen :wait t)
                   (clear-screen screen)))
      (let ((args (parse-args (next-line stream))))
        (switch line #'string=
          ("line" (apply #'add-edge edges args))
          ("scale" (apply #'scale (append args (list transform))))
          ("move" (apply #'translate (append args (list transform))))
          ("rotate" (apply #'rotate (append args (list transform))))
          ("save" (draw-lines edges screen '(255 0 255))
                  (apply #'save (string-downcase (symbol-name (first args)))
                         (list dimensions screen))
                  (clear-screen screen))
          (otherwise (format t "Unknown command: ~a~%" line))))))

(defun command-no-args (line)
  "Returns t if line takes no args. Nil otherwise."
  (or (string= line "ident") (string= line "apply") (string= line "display")))

(defun next-line (stream)
  "Reads the next line in stream. Returns \"quit\" if eof is reached."
  (read-line stream nil "quit"))

(defun parse-args (line)
  "Given LINE (a string), parse it into a list of args."
  (read-from-string (concatenate 'string "(" line ")")))

