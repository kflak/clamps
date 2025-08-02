;;; 
;;; clamps-utils.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2024 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(in-package :incudine)

(in-package :clamps)

(defgeneric clamps-dump (node &key stream control-list))
(defmethod clamps-dump ((obj incudine:node) &key (stream *logger-stream*) (control-list t))
  (declare (type stream stream))
  (let ((indent 0)
        (indent-incr 4)
        (last-list nil)
        (last (incudine::find-last-node obj)))
    (declare (type incudine::non-negative-fixnum indent indent-incr)
             #.*standard-optimize-settings*)
    (fresh-line stream)
    (flet ((inc-indent (n)
             (unless (symbolp (incudine::node-last n))
               (incf indent indent-incr)
               (push (incudine::find-last-node n) last-list)))
           (dec-indent (n)
             (loop while (eq n (car last-list)) do
               (pop last-list)
               (decf indent indent-incr)))
           (indent-line ()
             (do ((i 0 (1+ i)))
                 ((= i indent))
               (declare (type incudine::non-negative-fixnum i))
               (princ " " stream))))
      (incudine::dograph (n obj)
        (indent-line)
        (cond ((group-p n)
               (dec-indent n)
               (format stream "group ~D~@[ ~A~]~@[ (pause)~]~%"
                       (node-id n) (node-name n) (incudine::node-pause-p n))
               (inc-indent n))
              (t (if control-list
                     (progn
                       (format stream "node ~D~@[ (pause)~]~%" (node-id n)
                               (incudine::node-pause-p n))
                       (indent-line)
                       (reduce-warnings
                         (format stream "  ~A~{ ~A~}~%"
                                 (node-name n) (control-list n))))
                     (format stream "node ~D~@[ (pause)~]: ~A~%" (node-id n)
                             (incudine::node-pause-p n)
                             (node-name n)))
                 (dec-indent n)))
        (when (eq n last) (return)))
      (force-output stream))))

(defparameter %clamps-version% 1)

(defun clamps-version-number (&rest arg) arg %clamps-version%)

(defun clamps-version-name ()
  (format nil
          "~a.~a.~a"
          (ldb (byte 8 16) %clamps-version%)
          (ldb (byte 8 8) %clamps-version%)
          (ldb (byte 8 0) %clamps-version%)))

(defun clamps-version (&rest fmat)
  (cond ((null fmat)
         (format nil "Clamps ~a" (ou:system-version :clamps)))
        ((not (null (cdr fmat)))
         (error "clamps-version: more than one arg: ~s." fmat))
        ((eq (car fmat) ':number) %clamps-version%)
        ((eq (car fmat) ':string) (clamps-version-name))
        ((eq (car fmat) ':list)
         (list (ldb (byte 8 16) %clamps-version%)
               (ldb (byte 8 8) %clamps-version%)
               (ldb (byte 8 0) %clamps-version%)))
        (t (error "clamps-version: Bad format: ~s." (car fmat)))))

(defparameter *clamps-logo* t)

(defun clamps-logo ()
  "draw clamps logo on *standard-output* the nerdy way. Originally written
by Tobias Kunze. Some cleanup done by Orm Finnendahl."
  (if *clamps-logo*
      (let ((e "~%"))
        (format t e)
        (do ((v (make-string 15)) (y 0 (+ y 1)))
            ((= y 7) nil)
          (format t
                  (do ((x 0 (+ x 1)))
                      ((= x 15) (cond
                                  ((= y 2)
                                   (concatenate
                                    'string v " CLAMPS" e))
                                  ((= y 3)
                                   (concatenate
                                    'string v " Common Lisp Aided Music Production System" e))
                                  ((= y 4)
                                   (concatenate
                                    'string v " Version " (clamps-version-name) e))
                                  (t (concatenate 'string v e))))
                    (setf (elt v x)
                          (cond
                            ((<= 2 (- x y) 4) #\\) 
                            ((= (- x (- 4 (mod (+ 13 y) 15))) 1) #\/)
                            ((<= 1 y 5) #\-)
                            ((= (* (- x 6) (- y 3)) 15) #\/)
                            (:else #\SPACE))))))
        (format t e)))
  (values))

(defvar node nil)
(setf (fdefinition 'node) #'incudine:node)
(defvar bus nil)
(setf (fdefinition 'bus) #'incudine:bus)
(defsetf bus incudine::set-bus)

(defun set-page-dimensions (body width height &key (scale 1.0))
  "Set the page dimensions of the /body/ of a clog html page given its
/body/ in rem units. The minimum font-size of the HTML will be scaled
that in full screen display 1 rem equals 10 px. Use this function in
the gui creation function to change the aspect ratio of a page. The
optional /scale/ will scale the /width/ and /height/ of the page
proportionally, resuling in a reciprocal scaling of all CSS elements
using em or rem units.

@Arguments

body - Instance of clog:clog-body denoting the body element of the html page.

width -  Number denoting the page width in rem units.

height - Number denoting the page height in rem units.

scale - Number to scale width and height.

@Examples

;; For full HD with 16:9 aspect ratio:

(set-page-dimensions body 192 108)

;; For full HD with 8:5 aspect ratio:

(set-page-dimensions body 192 120)

;; For Quad HD with 16:9 aspect ratio:

(set-page-dimensions body 256 144)


"
  (let ((html (clog:document-element (html-document body))))
    (setf (style html :font-size) (format nil "min(~,8fvw, ~,8fvh)" (/ 100 (* scale width)) (/ 100 (* scale height))))
    (setf (style body :height) (format nil "~arem" (* height scale)))
    (setf (style body :width) (format nil "~arem" (* width scale)))))


#|
(defmacro imsg (type format-control &rest format-arguments)
  "Produce a formatted log message controlled by FORMAT-CONTROL and
FORMAT-ARGUMENTS.

TYPE should be one of ERROR, WARN, INFO or DEBUG."
  `(incudine.util::%msg ',(incudine::ensure-symbol type "INCUDINE.UTIL")
         ,format-control (list ,@format-arguments)))

(export '(reinit-midi restart-qsynth jack-connect-qsynth
          *mt-out01* *midi-in1* *midi-out1*
          start-cm-all cm-restart-gui reset-logger-stream
          *sly-connected-hooks* call-sly-connected-hooks
          install-standard-sly-hooks)
        'cm)
|#

(defun idump (&rest args)
  "Dump all active dsps of /node/ to the /incudine:*​logger-stream​*/
output.

@Arguments

node - Either a Non Negative Integer denoting the id of the node or an
/incudine:node/ Instance.
:control-list - flag indicating whether to output the control-list of the dsps.

@Note
If calling idump doesn't produce any output although dsps are running,
reset the logger-stream using <<reset-logger-stream>>.
"
  (let ((node (if (numberp (first args)) (pop args) 0))
        (control-list (getf args :control-list nil)))
    (unless incudine.util:*logger-stream*
      (setf incudine.util:*logger-stream* *error-output*))
    (clamps-dump (if (numberp node)
              (incudine:node node)
              node)
          :control-list control-list)))


(defun clamps:set-tempo (bpm)
  "Set the tempo in beats per minute for both, CM and Incudine.

@Arguments
bpm - Number of beats per minute.

@See-also
set-bpm
"  (setf cm:*tempo* bpm)
  (setf (bpm *tempo*) bpm))

(defparameter *clamps-doc-acceptor* (make-instance 'hunchentoot:easy-acceptor
        :port 8282
        :document-root (asdf:system-relative-pathname :clamps "doc/html/clamps-doc/")))

(defun start-doc-acceptor ()
  "Start the doc acceptor for online documentation. This is done
automatically on startup to make the clamps documentation
accessible at the URL /https://localhost:8282/.
"  (when (hunchentoot::acceptor-listen-socket *clamps-doc-acceptor*)
     (hunchentoot:stop *clamps-doc-acceptor*))  
  (hunchentoot:start *clamps-doc-acceptor*))

;;; (start-doc-acceptor)

(setf (fdefinition 'clamps::set-bpm) #'clamps:set-tempo)

;;; breakpoint versions of n-lin and n-exp

(defun n-lin-bp (x bp min max)
  (n-lin (apply #'interp x (flatten bp)) min max))

(defun n-exp-bp (x bp min max)
  (declare (float min max))
  (n-exp (apply #'interp x (flatten bp)) min max))

(defun plot-2d (seq)
  "Convenience wrapper around <<plot>>: A flat sequence of numbers is
interpreted as 2-d coordinate pairs.

@Examples
#+BEGIN_SRC lisp
(plot-2d '(2 1 4 3 6 10)) <=> (plot '((2 1) (4 3) (6 10)))
#+END_SRC
"
  (plot (ou:group seq 2))
  (values))

(defun plot-3d (seq)
  "Plot a flat sequence of coordinates by grouping the elements in 3."
  (plot (ou:group seq 3))
  (values))

(defvar *standard-pitch* 440.0
  "Tuning reference for /ftom/ and /mtof/ in Hz. Defaults to 440.

@Important-Note

Don't set this value directly! Rather use the <<standard-pitch>>
function with setf which changes the standard pitch reference for the
entire /Clamps/ system.

@See-also
ftom
mtof
standard-pitch
")

(defun set-standard-pitch (freq)
  "Set the ∗​standard-pitch​∗ reference of Clamps to freq in Hz.

@Arguments
freq - Frequency of A4 in Hz.

@See-also
standard-pitch
*keynum-offset*
"
  (setf *standard-pitch* (float freq 1.0))
  (setf oid::*standard-pitch* (float freq 1.0))
  (setf *keynum-offset* (fr->ct (/ *standard-pitch* 440)))
  freq)

(defun standard-pitch ()
  "Return the tuning reference of A4 in Hz. Setfable.

@Examples
(standard-pitch) ; => 440

(setf (standard-pitch) 415) ; => 415

(standard-pitch) ; => 415
"
  *standard-pitch*)

(defsetf standard-pitch set-standard-pitch)

#|

(defun ftom (f &key (tuning-base *standard-pitch*))
  "Convert frequency in Hz to pitch in Midicents.

@Arguments
freq - Frequency in Hz.
:tuning-base - Frequency of A4 in Hz.

@Examples

(ftom 440) ; => 69.0

(ftom 269.3) ; => 60.500526

(ftom 415 :tuning-base 415) ; => 69.0

@See-also
mtof
"
  (+ 69 (* 12 (log (/ f tuning-base ) 2))))

(defun mtof (midi-value &key (tuning-base *standard-pitch*))
  "Convert pitch in Midicts to frequency in Hz.

@Arguments
midi-value - Pitch in Midicents.
:tuning-base - Frequency of A4 in Hz.

@Examples
(mtof 69) ; => 440

(mtof 60.5) ; => 269.29178

(mtof 69 :tuning-base 415) ; => 415

@See-also
ftom
"
(* tuning-base (expt 2 (/ (- midi-value 69) 12))))
|#

(defun fr2ct (ratio)
       "Return the Midicents interval of /ratio/.

@Arguments
ratio - The frequency ratio of the interval.
@Examples
#+BEGIN_SRC lisp
(fr2ct 2) ;; => 12.0

(fr2ct 4/5) ;; => -3.863137

(fr2ct 3/2) ;; => 7.01955

(fr2ct 1/2) ;; => -12.0
#+END_SRC

@See-also
ct2fr
"
  (* 12 (log ratio 2)))

(defun ct2fr (ct)
  "Return the frequency ratio of the Midicents interval /cent/.

@Arguments
cent - The interval in Midicents.
@Examples
#+BEGIN_SRC lisp
(ct2fr 12) ;; => 2

(ct2fr 1) ;; => 1.0594631

(ct2fr 7) ;; => 1.4983071

(ct2fr -12) ;; => 1/2
#+END_SRC

@See-also
fr2ct
"
  (expt 2 (/ ct 12)))

;;; wrappers for cm eventclass slots:

(defun evt-duration (obj)
  "Return the duration of CM /obj/.

@Arguments
obj - Instance of a Common Music event

@Example
(evt-duration (new midi)) ; => 0.5

@See-also
evt-keynum
evt-time
evt-amp
"
  (sv obj :duration))

(defun evt-keynum (obj)
  "Return the keynum of CM /obj/.

@Arguments
obj - Instance of a Common Music event

@Example
(evt-keynum (new midi)) ; => 60

@See-also
evt-amp
evt-duration
evt-time
"
  (sv obj :keynum))

(defun evt-time (obj)
  "Return the time of CM /obj/.

@Arguments
obj - Instance of a Common Music event

@Example
(evt-time (new sfz :time 3)) ; => 3

@See-also
evt-amp
evt-duration
evt-keynum
"
  (sv obj :time))

(defun evt-amp (obj)
  "Return the amplitude of CM /obj/.

@Arguments
obj - Instance of a Common Music event

@Example
(evt-amp (new midi)) ; => 0.5

@See-also
evt-duration
evt-keynum
evt-time
"
  (sv obj :amplitude))

(defmacro clamps-bounce-to-disk ((output-filename
                                  &key input-filename
                                    (channels *number-of-output-bus-channels*)
                                    duration (pad 2) (sample-rate *sample-rate*)
                                    header-type data-format metadata)
                                 &body body)
  "Wrapper around incudine:bounce-to-disk, temporarily setting clamps'
*​standard-output-group​* to 0."
  `(let ((of-incudine-dsps::*standard-output-group* 0))
     (bounce-to-disk (,output-filename
                      :input-filename ,input-filename
                      :channels ,channels
                      :duration ,duration :pad ,pad
                      :sample-rate ,sample-rate
                      :header-type ,header-type :data-format ,data-format
                      :metadata ,metadata)
       ,@body)))

(defmacro imsg (type format-control &rest format-arguments)
  "Produce a formatted log message controlled by FORMAT-CONTROL and
FORMAT-ARGUMENTS.

TYPE should be one of ERROR, WARN, INFO or DEBUG."
  `(incudine.util::%msg ',(incudine::ensure-symbol type "INCUDINE.UTIL")
         ,format-control (list ,@format-arguments)))

