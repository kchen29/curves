(defun main-test ()
  "Tests drawing circles and curves."
  (let* ((screen-size 500)
         (dimensions (list screen-size screen-size))
         (screen (make-array dimensions :initial-element '(0 0 0)))
         (edges (make-matrix 4 0)))
    (draw-circle edges .01 250 250 0 150)
    (draw-hermite edges .01 150 150 350 150 -100 -100 100 150)
    (draw-bezier edges .01 23 29.7 9 500 475.2 486 448 -16)
    
    (draw-lines edges screen '(255 0 255))
    (display dimensions screen)))

(defun main (filename)
  "Sets up then parses FILENAME. See parser.lisp."
  (let* ((screen-size 500)
         (dimensions (list screen-size screen-size))
         (screen (make-array dimensions :initial-element '(0 0 0)))
         (edges (make-matrix 4 0))
         (transform (make-matrix)))
    (parse-file filename edges transform dimensions screen)))
