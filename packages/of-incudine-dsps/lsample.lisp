;;; 
;;; lsample.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2021 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(in-package :of-incudine-dsps)

(unless (boundp 'cl-user:*sfile-path*)
  (defparameter cl-user:*sfile-path* nil))

(defparameter *standard-output-group* 200
  "standard group for output.")

(defvar *standard-pitch* 440.0
  "Reference tuning frequency for middle A, setfable.")

(defparameter *keynum-offset* 0
  "Keynum offset related to the ratio of <<standard-pitch>> in relation to
440 Hz. Gets readjusted by setf of standard-pitch in clamps.

@See-also
standard-pitch")

(defstruct lsample
  "Structure for a sample with two loop-points.

@Note
Normally the user shouldn't be dealing with a lsample struct
directly. It is used by the /sfz/ and /poolevent/ classes and
documented here for completeness and insight.

A lsample contains the following slots, accessible using the functions
/lsample-<slot-name>/:

=name= -- Filename of the sample source.

=buffer= -- Buffer of the sample data.

=oneshot= -- Boolean indicating whether not to loop the sample on playback.

=keynum= -- Double Float denoting original keynum of the recorded sample.

=loopstart= -- Double Float denoting the loop start for loop playback, defaulting to /+​sample-zero​+/.

=loopend= -- Double Float denoting the loop start for loop playback, defaulting to /+​sample-zero​+/.

=amp= -- Amplitude of recorded sample in dB, defaulting to /+​sample-zero​+/.

@See-also
poolevt
sfz
"
  name
  buffer
  (oneshot nil :type boolean)
  (keynum +sample-zero+ :type sample)
  (loopstart +sample-zero+ :type sample)
  (amp (sample 0) :type sample)
  (loopend +sample-zero+ :type sample))

(defun create-lsample (file &rest args)
  "Return a lsample instance from /file/ and /args/

@Arguments
file - Pathname or String denoting filename.
:path - List of Pathnames to search for file. Defaults to [[#sfile-path][*​sfile-path​*]].
:keynum - Number denoting keynum of sample.
:oneshot - Boolean indicating whether not to loop the sample. Defaults to t.
:loopstart - Positive Integer denoting start of loop. Defaults to 0.
:loopend - Positive Integer denoting end of loop. Defaults to 0, denoting the end of the sample.
:amp - Number denoting amplitude in dB. The range [-100..0] is mapped to linear amplitude [0..1]. Defaults to 0.

@See-also
make-lsample
"
  (let* ((path (or (getf args :path) cl-user:*sfile-path*))
         (name (if (pathnamep file) (file-namestring file) file))
         (filename (if (pathnamep file) (namestring file) file))
         (oneshot (or (getf args :oneshot) t)))
    (remf args :path)
    (remf args :oneshot)
    (arglist-ansure-samples (:amp :keynum :loopstart :loopend) args)
    (apply #'make-lsample
           :name name
           :buffer (incudine-bufs:clamps-buffer-load filename :path path)
           :oneshot oneshot
           args)))

(defun lsample-pathname (lsample)
  "Return the full pathname of /lsample/.

@Arguments
lsample - Instance of type lsample.

@See-also
lsample
lsample-dur
"
  (pathname
   (incudine:buffer-file (lsample-buffer lsample))))

(defun lsample-dur (lsample)
  "Return duration of /lsample/ in seconds.

@Arguments

lsample - Lsample Struct

@See-also

lsample
lsample-pathname
"
  (buffer-dur (lsample-buffer lsample)))

(defun db->lin (value)
  "Convert VALUE dB to linear value."
  (expt (sample 10) (* value (sample 0.05))))

(declaim (inline keynum->hz))
(defun keynum->hz (keynum)
  "Convert VALUE dB to linear value."
  (* (sample *standard-pitch*) (expt 2 (/ (- keynum 69.0d0) 12.0d0))))

(defun play-lsample (lsample pitch ampdb duration &key (pan 0.5) (startpos 0) (oneshot nil osp) out1 out2)
  "Play /lsample/ with given /pitch/, /ampdb/ and /duration/, /pan/ and
/startpos/ with or without looping.

@Arguments
lsample - Instance of type lsample
pitch - Positive Number denoting pitch in Midicents.
ampdb - Positive number denoting amp in dB, mapping range [-100..0] to linear amp [0..1].
duration - Positive number denoting duration in seconds.
:pan - Number denoting equal power panorama in the range [0..1].
:startpos - Positive number denoting start position into the sample in seconds.
:oneshot - Boolean indicating whether not to loop the sample on playback.

@See-also
lsample
"
  (with-slots (buffer amp keynum loopstart loopend) lsample
    (let* ((rate (if pitch (incudine::sample (ou:ct->fr (+ *keynum-offset* (- pitch keynum)))) 1))
           (oneshot (if osp oneshot (lsample-oneshot lsample)))
           (out1 (or out1 0))
           (out2 (or out2 (1+ out1))))
      (if oneshot
        (play-buffer* buffer oid:*env1* duration (+ amp ampdb) rate pan startpos out1 out2 :tail *standard-output-group*)
        (play-buffer-loop* buffer oid:*env1* duration (+ amp ampdb) rate pan loopstart loopend startpos out1 out2 :tail *standard-output-group*)))))

(defun buffer->lsample (buffer &key (keynum 60) (ampdb 0) (oneshot t)
                                 (loopstart 0) (loopend 0))
  "Return a lsample struct of /buffer/.

@Arguments
buffer - Incudine buffer to convert.
:keynum - keynum of the sound in the buffer.
:ampdb - Positive number denoting amp in dB, mapping range [-100..0] to linear amp [0..1].
:oneshot - Boolean indicating whether not to loop the sample on playback.
:loopstart - Positive number denoting the loop start in seconds.
:loopend - Positive number denoting the loop end in seconds.
"
  (make-lsample :buffer buffer :name (buffer-name buffer)
                :keynum (float keynum 1.0d0)
                :amp (float ampdb 1.0d0) :oneshot oneshot
                :loopstart (float loopstart 1.0d0)
                :loopend (float loopend 1.0d0)))

(defun load-all-lsamples (parent-dir &key (hashtable (make-hash-table)) exclude )
  "Return a hash table with arrays of lsamples of all soundfiles in all
subdirectories of parent-dir. The keys of the hash table are keywords
of the subdirectory names.

@Arguments
parent-dir - Pathname of a directory to recursively search for soundfile directories.
:hashtable - Hashtable to store the retrieved lsamples.
:exclude - List of strings of directories to exclude.
"
  (dolist (dir (uiop:subdirectories parent-dir))
    (format t "~&checking: ~a  ~%~a" dir (uiop:directory-files dir "*.wav"))
    (when (and (uiop:directory-files dir "*.wav")
               (not (member (relative-directory-name dir) exclude :test #'string=))) ;;; only register non-empty dirs which aren't excluded.
      (setf (gethash (relative-directory-keyword dir) hashtable)
            (coerce
             (loop for file in (uiop:directory-files dir "*.wav")
                   do (format t "~&adding: ~A" file)
                   collect (buffer->lsample (clamps-buffer-load file)))
             'vector)))
    (load-all-lsamples dir :hashtable hashtable :exclude exclude))
  hashtable)
