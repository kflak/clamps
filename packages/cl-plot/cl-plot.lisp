;;; 
;;; cl-plot.lisp
;;;
;;; simple interface to gnuplot. Depends on uiop version > 3.2.
;;;
;;; **********************************************************************
;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;;
;;; Revision history: See git repository.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Gnu Public License, version 2 or
;;; later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
;;; of this agreement.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; **********************************************************************

(in-package :cl-plot)

(defparameter *gnuplot-program* "/usr/bin/gnuplot")
(defparameter *gnuplot-options* "notitle with lines;")
(defparameter *gnuplot-header* nil)

;;; map-indexed utility to make plot more generic (using map rather than loop)

(defmacro map-indexed (result-type fn data)
  "map across a sequence by (funcall fn idx elem) on all elems of
sequence with incrementing idx (similar to the clojure function with
the same name, but not lazy and with additional return type like
Common Lisp's #'map."
  (let ((idx (gensym)))
    `(let ((,idx -1))
       (map ,result-type (lambda (elem) (funcall ,fn (incf ,idx) elem)) ,data))))

(defun construct-plot-command (&key region (grid t) (header *gnuplot-header*) (options *gnuplot-options*) (3d nil) &allow-other-keys)
  "Helper function to construct the gnuplot command with a given
header, options and a grid flag."
  (concatenate 'string
               (if grid (format nil "set grid xtics lt 1 lc rgb \"#bbbbbb\";set grid ytics lt 1 lc rgb \"#bbbbbb\";~%") "")
               (if header (format nil "~a~%" header) "")
               (if 3d "splot " "plot ")
               (if region (format nil "[~{~,2f~^:~}] " region) "")
               "'<cat' "
               options
               "; pause mouse close"))

(defun launch-gnuplot (&rest args &key region &allow-other-keys)
  (uiop:launch-program
   (list *gnuplot-program*  "-p" "-e"
         (apply #'construct-plot-command :region region args))
   :input :stream))

(defgeneric plot (data &rest args &key region header options grid 3d &allow-other-keys)
  (:documentation "Plot /obj/ using <<http://www.gnuplot.info/><GnuPlot>>.

@Arguments

obj - The object to be plotted. Currently the following object
types are implemented:

- =seq= A sequence of numbers, interpreted as y-values of
successive x-values starting at 0.

Pairs as elements of /seq/ are interpreted as 2d coordinates of
data points. Vectors, arrays or lists are valid sequences.

- =Function= A function of one argument. Displays the values of
applying function to x-values in the range /[0..1]/.


- =incudine:buffer= Display the contents of an incudine
buffer. For a sample buffer this acts like a waveform display,
but any buffer data can be displayed.

:region - A list of two values defining the left and right margin of
x-values of the plot.

:header - A string supplied as a header to GnuPlot before initiating
the plot command.

:options - A string with options for GnuPlot.
:grid - Boolean indicating whether to use a grid.

@Examples-nosrc
#+BEGIN_SRC lisp
(plot '(5 4 6 1 9)) ; => (5 4 6 1 9)
#+END_SRC
#+attr_html: :width 50%
#+CAPTION: output of (plot '(5 4 6 1 9))
[[./img/plot-01.svg]]
#+BEGIN_SRC lisp
(plot '((-2 5) (0 8) (4 -2) (6 10)))  ; => ((-2 5) (0 8) (4 -2) (6 10))
#+END_SRC
#+attr_html: :width 50%
#+CAPTION: output of (plot '((-2 5) (0 8) (4 -2) (6 10)))
[[./img/plot-02.svg]]
#+BEGIN_SRC lisp
(defun my-fn (x) (* x x)) ; => my-fn

(plot #'my-fn)  ; => #<function my-fn>
#+END_SRC
#+attr_html: :width 50%
#+CAPTION: output of (plot #'my-fn)
[[./img/plot-03.svg]]
#+BEGIN_SRC lisp
(plot #'my-fn :region '(-10 10)) ; => #<function my-fn>
#+END_SRC
#+attr_html: :width 50%
#+CAPTION: output of (plot #'my-fn :region '(-10 10))
[[./img/plot-04.svg]]
#+BEGIN_SRC lisp
(ensure-sfz-preset :flute-nv)

(plot (first (sfz-preset-buffer :flute-nv 60)))
#+END_SRC
#+attr_html: :width 50%
#+CAPTION: output of (plot (first (sfz-preset-buffer :flute-nv 60)))
[[./img/plot-05.svg]]

@See-also
plot-2d
"))

;;;  default function for the :data-fn arg of plot. The Return value
;;;  should be of the format (values x y &rest z ...)

(defun plot-2d (data &rest args &key region header options grid &allow-other-keys)
  (declare (ignorable region header options grid))
  (apply #'plot (ou:group data 2) args))

(defun gnuplot-data-fn (idx obj)
  (if (numberp obj)
      (values idx obj)
      (let ((lst (coerce obj 'list)))
        (cond ((consp (cdr lst)) (apply #'values lst))
              ((numberp (first lst)) (values idx (first lst)))
              (:else (error "value not a number: ~a" (first lst)))))))

(declaim (inline get-first))
(defun get-first (seq)
  (elt seq 0))

(defun get-first-min-max (seq)
  "return the min and max values of the first elements of the subseqs
in seq."
  (let ((min (get-first (get-first seq)))
        (max (get-first (get-first seq))))
    (map
     nil
     (lambda (x) (let ((num (get-first x)))
              (setf min (min num min))
              (setf max (max num max))))
     seq)
    (list min max)))

#|

alternative (consing) definition:

(defun get-first-min-max (seq)
  "return the min and max values of the first elements of the subseqs
in seq."
  (let ((x-vals (map 'list #'get-first seq)))
    (list
     (float (apply #'min x-vals) 1.0)
     (float (apply #'max x-vals) 1.0))))

|#

(defmacro with-gnuplot-instance ((stream &rest args)
                                 &body body)
  "start an external gnuplot process with a data input stream open for the extent of body.

stream is bound to gnuplot's input stream. Printing to it is
equivalent to printing into a file read by gnuplot as a dataset with
its plot command.

args are arguments sent to #'launch-gnuplot. 

Leaving the macro is equivalent to gnuplot reaching EOF when reading
an external dataset."
  (let ((gnuplot-instance (gensym)))
    `(let ((,gnuplot-instance (apply #'launch-gnuplot ,args)))
       (with-open-stream (,stream (uiop:process-info-input ,gnuplot-instance))
         ,@body))))

(defmethod plot ((data sequence) &rest args &key region
                                                (header *gnuplot-header*)
                                                (data-fn #'gnuplot-data-fn)
                                                (options *gnuplot-options*) (grid t))
  "Plot input data given as a sequence. The :data-fn key specifies a
  function which is applied to each element of the sequence (with its
  idx as second argument) and should return the data of one gnuplot
  dataset as values. The default data-fn handles numbers in the
  sequence as y values and their index is taken as x value. In case
   the sequence is comprised of subsequences, the elements of the
  subseqs are interpreted as (x y &rest z...) values. 

  plot returns the original data sequence."
    (declare (ignore header options grid))
    (setf (getf args :region)
          (or region (if (numberp (get-first data))
                         `(0 ,(1- (length data)))
                         (get-first-min-max data))))
    (with-gnuplot-instance (out . args)
      (map-indexed nil (lambda (idx x)
                         (format out "~{~,4f~^ ~}~%"
                                 (multiple-value-list (funcall data-fn idx x))))
                   data))
    (values data))

#|
     Examples:

     (plot '(3 1 8 6 5 2 4 3 1 2 5 6))

     (plot '((0 3) (1 1) (2 8) (3 6) (4 5) (5 2) (6 4)))

     (plot '(#(0 3) #(1 1) #(2 8) #(3 6) #(4 5) #(5 2) #(6 4)))

     (plot #(3 1 8 6 5 2 4))

     (plot #((0 3) (1 1) (2 8) (3 6) (4 5) (5 2) (6 4)))

     (plot #(#(0 3) #(1 1) #(2 8) #(3 6) #(4 5) #(5 2) #(6 4)))

|#

(defmethod plot ((fn function) &rest args
                 &key (region '(0 1)) (header *gnuplot-header*)
                   (options *gnuplot-options*) (num-values 100) (grid t)
                   &allow-other-keys)
  "Plot function fn (fn has to be a function accepting 1 number argument). 

:region specifies xmin and xmax (default (0 1)),

:num-values the number of values to plot (default 100). 

Return the original function."
  (declare (ignore header options grid))
  (with-gnuplot-instance (out . args)
    (destructuring-bind (xmin xmax) region
      (loop
         for count below (1+ num-values)
         collect
           (let ((x (+ xmin (/ (* count (- xmax xmin)) num-values))))
             (format out "~,4f ~,4f~%" x (funcall fn x))))))
  (values fn))

#|

Examples:

(plot #'sin :region `(0 ,(* 2 pi)))

(plot #'exp :region `(0 4))

;; default region is '(0 1)

(plot #'sin)

|#
