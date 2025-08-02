;;; 
;;; sfz-lsample.lisp
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

(in-package :cl-sfz)

(defun abs-path (sample-path sfz-file-path)
  "Return the full path of a sample entry in a sfz file.

@Arguments
sample-path - String from the sample path definition of a sfz file.
sfz-file-path - Pathname denoting the location of the sfz file or its directory.

@See-also
sfz
sfz->lsample
"
  (merge-pathnames sample-path sfz-file-path))

(defun get-keynum (entry)
  (incudine::sample (- (or (getf entry :pitch-keycenter) 60) (/ (or (getf entry :tune) 0) 100))))

(defun sfz->lsample (sfz-entry dir &key oneshot)
  "Convert an entry of a sfz file into a lsample.

@Arguments
sfz-entry - Instance of sfz class.
dir - Pathname or String denoting the directory of the sfz file.
:oneshot - Boolean denoting whether not to loop the playback.

@See-also
sfz
lsample
"
  (let* ((abs-filepath (abs-path (getf sfz-entry :sample) (pathname dir)))
         (buffer (incudine-bufs:clamps-buffer-load abs-filepath)))
    (of-incudine-dsps:make-lsample
     :name (file-namestring abs-filepath)
     :buffer buffer
     :oneshot oneshot
     :keynum (get-keynum sfz-entry)
     :amp (incudine.util:sample (getf sfz-entry :volume 0))
     :loopstart (incudine.util:sample (or (getf sfz-entry :loop-start) 0))
     :loopend (incudine.util:sample (or (getf sfz-entry :loop-end) (buffer-frames buffer))))))

(declaim (inline get-lsample))
(defun get-lsample (keynum map)
  (aref map (min (round keynum) 127)))
