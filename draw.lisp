(defun plot (x y screen color)
  "Plots (x, y) on the 2D array SCREEN with COLOR.
   Rounds x and y. Checks bounds. COLOR is not copied."
  (setf x (round x) y (round y))
  (when (and (< -1 x (array-dimension screen 0)) (< -1 y (array-dimension screen 1)))
    (setf (aref screen x y) color)))

(defmacro draw-line-base (x0 y0 x1 y1 plot-1 plot-2)
  "Base code for octant 1. Other octants can be gotten from transformations."
  `(do* ((x ,x0 (1+ x))
         (y ,y0)
         (A (- ,y1 ,y0))
         (B (- ,x0 ,x1))
         (2A (* 2 A))
         (2B (* 2 B))
         (d (+ 2A B) (+ d 2A)))
        ((> x ,x1))
     (plot ,plot-1 ,plot-2 screen color)
     (when (> d 0)
       (incf y)
       (incf d 2B))))

(defun draw-line (x0 y0 x1 y1 screen color)
  "Draws a line from (x0, y0) to (x1, y1) on SCREEN using COLOR."
  (when (minusp (- x1 x0))
    (rotatef x0 x1)
    (rotatef y0 y1))
  (let ((xdif (- x1 x0))
        (ydif (- y1 y0)))
    (if (>= ydif 0)
        (if (minusp (- ydif xdif))
            (draw-line-base x0 y0 x1 y1 x y)
            (draw-line-base y0 x0 y1 x1 y x))
        (if (minusp (+ ydif xdif))
            (draw-line-base y0 x0 (- y0 ydif) x1 y (- (* 2 y0) x))
            (draw-line-base x0 y0 x1 (- y0 ydif) x (- (* 2 y0) y))))))

(defun draw-lines (edges screen color)
  "Draws the lines from EDGES onto SCREEN with COLOR."
  (loop while (car edges)
        for p0 = (pop edges)
        for p1 = (pop edges)
        do (draw-line (first p0) (second p0) (first p1) (second p1) screen color)))

(defun add-edge (edges x0 y0 z0 x1 y1 z1)
  "Adds a line from point (x0 y0 z0) to (x1 y1 z1) to EDGES."
  (add-point edges x1 y1 z1)
  (add-point edges x0 y0 z0))

(defun add-point (edges x y z)
  "Adds a point (x y z) to EDGES."
  ;;can't change edges directly, caller's edges would stay the same
  ;;edges starts out as a cons of nil
  (when (car edges)
    (setf (cdr edges) (cons (car edges) (cdr edges))))
  (setf (car edges) (list x y z 1)))

(defun draw-parametric (edges step x-function y-function &optional (z 0))
  "Given X-FUNCTION and Y-FUNCTION, which take one input and outputs the x and y
   coordinates respectively, draw a parametric where s runs from 0 to 1 at STEP interval
   and add the connecting lines to EDGES.
   Optionally takes a z value, defaulted to 0, where all the points are shifted by z."
  (flet ((get-x (s) (funcall x-function s))
         (get-y (s) (funcall y-function s)))
    (do* ((s step (+ s step))
          (prev-x (get-x 0) x)
          (prev-y (get-y 0) y)
          (x (get-x s) (get-x s))
          (y (get-y s) (get-y s)))
         ((>= s (1+ step)))
      (add-edge edges prev-x prev-y z x y z))))

(defun draw-circle (edges step x y z radius)
  "Draws a circle to EDGES with center (x y) and RADIUS with STEP interval. Circle shifted by Z."
  (draw-parametric edges step
                   (lambda (s) (+ x (* radius (cos (* 2 pi s)))))
                   (lambda (s) (+ y (* radius (sin (* 2 pi s)))))
                   z))

(defun evaluate-polynomial (x &rest coefficients)
  "Evaluates a polynomial in X with COEFFICIENTS. Starts from the least power (x^0) and
   increases with each coefficient."
  (loop for coeff in coefficients
        for product = 1 then (* x product)
        sum (* coeff product)))

(defun draw-hermite (edges step x0 y0 x1 y1 dx0 dy0 dx1 dy1)
  "Draws a hermite curve to EDGES with points (x0 y0) and (x1 y1) and the rates wrt. time of
   the corresponding coordinates (dx0 dy0) and (dx1 dy1), with STEP interval."
  (draw-parametric edges step
                   (get-hermite-cubic x0 x1 dx0 dx1)
                   (get-hermite-cubic y0 y1 dy0 dy1)))

(defun get-hermite-cubic (x0 x1 dx0 dx1)
  "Returns the function, given the coordinate (x0 x1) and rates of changes (dx0 dx1),
   taking in a time and returning the output on a hermite cubic curve."
  (lambda (s) (evaluate-polynomial s
                                   x0 dx0
                                   (- (* 3 x1) (* 3 x0) (* 2 dx0) dx1)
                                   (+ (* 2 x0) (* -2 x1) dx0 dx1))))

(defun draw-bezier (edges step x0 y0 x1 y1 x2 y2 x3 y3)
  "Draws a bezier curve to EDGES with endpoints (x0 y0) and (x3 y3).
   (x1 y1) and (x2 y2) are control points. Drawn with STEP interval."
  (draw-parametric edges step
                   (get-bezier-cubic x0 x1 x2 x3)
                   (get-bezier-cubic y0 y1 y2 y3)))

(defun get-bezier-cubic (x0 x1 x2 x3)
  "Returns the function, given the x coordinates, taking in a time and returning the output
   on a bezier cubic curve."
  (lambda (s) (evaluate-polynomial s
                                   x0 (* 3 (- x1 x0))
                                   (- (* 3 (+ x0 x2)) (* 6 x1))
                                   (+ (* 3 (- x1 x2)) (- x3 x0)))))
