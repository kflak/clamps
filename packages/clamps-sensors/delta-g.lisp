(in-package :clamps-sensors)

(defparameter *deltatriggers* (make-hash-table :test #'equal))

(defclass acc-g-data ()
  ((x :initform 0.0 :accessor :x)
   (y :initform 0.0 :accessor :y)
   (z :initform 0.0 :accessor :z)
   (delta :initform 0.0 :accessor :delta)
   (prev-x :initform 0.0 :accessor :prev-x)
   (prev-y :initform 0.0 :accessor :prev-y)
   (prev-z :initform 0.0 :accessor :prev-z)))

(defun calc-delta (&key (sensor (find-sensor :sensor1)))
  (let ((data (make-instance 'acc-g-data)))
    (push
     (watch (lambda ()
              (let* ((new-x (sensor-gx sensor))
                     (new-y (sensor-gy sensor))
                     (new-z (sensor-gz sensor)))
                (setf (:x data) new-x)
                (setf (:y data) new-y)
                (setf (:z data) new-z)

                (setf (:delta data)
                      (+ (abs (- new-x (:prev-x data)))
                         (abs (- new-y (:prev-y data)))
                         (abs (- new-z (:prev-z data)))))

                (setf (sensor-delta-g sensor) (:delta data))

                (setf (:prev-x data) new-x)
                (setf (:prev-y data) new-y)
                (setf (:prev-z data) new-z))))
     (sensor-unwatch sensor))))

(defclass deltatrigger ()
  ((threshold
    :initarg :threshold
    :initform 0.1
    :accessor :threshold
    :documentation "The threshold for triggering an event")
   (speedlim
    :initarg :speedlim
    :initform 0.1
    :accessor :speedlim
    :documentation "The time it takes before an action can be retriggered")
   (action
    :initarg :action
    :initform (lambda () nil)
    :accessor :action
    :documentation "The function to be triggered")
   (active-p
    :initarg :active-p
    :initform t
    :accessor :active-p
    :documentation "Check if the deltatrigger is active")
   (trigger-time
    :initarg :trigger-time
    :initform (now)
    :accessor :trigger-time
    :documentation "The last time action was triggered")))

(defun trigger-action (action deltatrigger sensor)
  (when (:active-p deltatrigger)
    (when (and (sensor-delta-g sensor)
               (> (- (now) (:trigger-time deltatrigger))
                  (:speedlim deltatrigger)))

      (funcall action)
      (setf (:trigger-time deltatrigger) (now)))
    (let ((resample-interval 1/20))
      (at (+ (now) resample-interval)
          #'trigger-action action deltatrigger sensor))))

(defun start-deltatrigger (name action &key (threshold 0.1) (speedlim 0.1) (sensor :sensor1))
  (setf (gethash name *deltatriggers*)
        (make-instance 'deltatrigger
                       :threshold threshold
                       :speedlim speedlim
                       :action action))
  (trigger-action action (gethash name *deltatriggers*) sensor))

(defun stop-deltatrigger (name)
  (setf (:active-p (gethash name *deltatriggers*)) nil))

(defun set-threshold (threshold deltatrigger)
  (setf (:threshold deltatrigger) threshold))
