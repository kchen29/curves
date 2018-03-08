(defun main ()
  "Tests draw-parametric."
  (let* ((screen-size 500)
         (dimensions (list screen-size screen-size))
         (screen (make-array dimensions :initial-element '(0 0 0)))
         (edges (make-matrix 4 0)))
    (draw-parametric (circle-x 250 150) (circle-y 250 150) .05 edges)
    ;; (draw-parametric (lambda (s) ))
    
    (draw-lines edges screen '(255 0 255))
    (display dimensions screen)))
