(defun draw-circle (edges x y z r radian)
  "Draws a circle to EDGES with center (x y z) and radius R along the xy plane.
   Approximated using many lines, spaced RADIAN apart."
  ;;special-case for tidiness
  (when (zerop r)
    (add-edge edges x y z x y z)
    (return-from draw-circle))
  (do* ((x-val (+ x r) (+ x delx))
        (y-val y (+ y dely))
        (theta radian (+ theta radian))
        (delx (* r (cos theta)) (* r (cos theta)))
        (dely (* r (sin theta)) (* r (sin theta))))
       ((> theta (* 2 pi))
        (add-edge edges x-val y-val z (+ x r) y z))     ;join last line
    (add-edge edges x-val y-val z (+ x delx) (+ y dely) z)))

;;wip... Probably not how it should be done.
(defun draw-sphere (edges x y z r radian spaced)
  "Draws a sphere to EDGES with center (x y z) and radius R.
   Approximated using many lines spaced RADIAN apart.
   Circles are SPACED apart"
  ;;Draw from the center, then translate
  ;;Draw circle, rotate, then draw again
  (do* ((cur-z r (- cur-z spaced))
        (cur-r 0 (sqrt (- (* r r) (* cur-z cur-z)))))
       ((< cur-z (- 0 r)))
    (draw-circle edges 0 0 cur-z cur-r radian))
  (translate x y z edges))

(defun edges-to-script (edges stream)
  "Write EDGES to the script via STREAM."
  (do ((i 0 (+ 2 i)))
      ((>= i (1- (array-dimension edges 1))))
    (format stream "line~%~a ~a ~a ~a ~a ~a~%"
            (round (aref edges 0 i))
            (round (aref edges 1 i))
            (round (aref edges 2 i))
            (round (aref edges 0 (1+ i)))
            (round (aref edges 1 (1+ i)))
            (round (aref edges 2 (1+ i))))))

(defun main-script ()
  (let ((edges (make-matrix 4 0)))
    (draw-sphere edges 250 250 0 150 (/ pi 14) 10)
    (with-open-file (stream "script-circle" :direction :output :if-exists :supersede)
      (edges-to-script edges stream)
      (format stream "display~%ident~%move~%-250 -250 0~%rotate~%x 45~%move~%250 250 0~%apply~%display~%save~%circles.png"))
    (main "script-circle")))
