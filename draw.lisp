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

(defun draw-lines (matrix screen color)
  "Draws the lines from MATRIX onto SCREEN with COLOR."
  (do ((index 0 (+ 2 index)))
      ((>= index (array-dimension matrix 1)))
    (draw-line (aref matrix 0 index)
               (aref matrix 1 index)
               (aref matrix 0 (1+ index))
               (aref matrix 1 (1+ index))
               screen color)))

(defun add-edge (matrix x0 y0 z0 x1 y1 z1)
  "Adds a line from point (x0 y0 z0) to (x1 y1 z1)."
  (add-point matrix x0 y0 z0)
  (add-point matrix x1 y1 z1))

(defun add-point (matrix x y &optional (z 0))
  "Adds a point (x y z) onto MATRIX.
   Appends the point as a column."
  (adjust-array matrix (list (array-dimension matrix 0)
                             (1+ (array-dimension matrix 1))))
  (let ((end (1- (array-dimension matrix 1))))
    (setf (aref matrix 0 end) x
          (aref matrix 1 end) y
          (aref matrix 2 end) z
          (aref matrix 3 end) 1)))

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
                   (lambda (s) (+ (* radius (cos (* 2 pi s))) x))
                   (lambda (s) (+ (* radius (sin (* 2 pi s))) y))
                   z))

(defmacro polynomial-expand (x &rest coefficients)
  "Returns a polynomial in X with COEFFICIENTS. Starts from the least power (x^0) and
   increases with each coefficient."
  `(+ ,@(loop for coeff in coefficients
              for i = -1 then (1+ i)
              if (= i -1)
                collect coeff
              else
                collect `(* ,coeff ,@(loop for j upto i
                                           collect x)))))

(defun draw-hermite (edges step x0 y0 x1 y1 dx0 dy0 dx1 dy1)
  "Draws a hermite curve to EDGES with points (x0 y0) and (x1 y1) and the rates wrt. time of
   the corresponding coordinates (dx0 dy0) and (dx1 dy1), with STEP interval."
  (draw-parametric edges step
                   (get-hermite-cubic x0 x1 dx0 dx1)
                   (get-hermite-cubic y0 y1 dy0 dy1)))

(defun get-hermite-cubic (x0 x1 dx0 dx1)
  "Returns the function, given the coordinate (x0 x1) and rates of changes (dx0 dx1),
   taking in a time and returning the output on a hermite cubic curve."
  (lambda (s) (polynomial-expand s
                                 x0 dx0
                                 (- (* 3 x1) (* 3 x0) (* 2 dx0) dx1)
                                 (+ (* 2 x0) (- 0 (* 2 x1)) dx0 dx1))))

(defun draw-bezier (edges step x0 y0 x1 y1 x2 y2 x3 y3)
  "Draws a bezier curve to EDGES with endpoints (x0 y0) and (x3 y3).
   (x1 y1) and (x2 y2) are control points. Drawn with STEP interval."
  (draw-parametric edges step
                   (get-bezier-cubic x0 x1 x2 x3)
                   (get-bezier-cubic y0 y1 y2 y3)))

(defun get-bezier-cubic (x0 x1 x2 x3)
  "Returns the function, given the x coordinates, taking in a time and returning the output
   on a bezier cubic curve."
  (lambda (s) (polynomial-expand s
                                 x0 (* 3 (- x1 x0))
                                 (- (* 3 (+ x0 x2)) (* 6 x1))
                                 (+ (* 3 (- x1 x2)) (- x3 x0)))))
