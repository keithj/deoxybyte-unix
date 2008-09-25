;;;
;;; Copyright (C) 2007-8, Keith James. All rights reserved.
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;

(in-package :cl-mmap-test)

(defparameter *ascii-characters* (loop
                                    for i from 32 to 126
                                    collect (code-char i)))

(deftestsuite cl-mmap-tests ()
  ())

(addtest (cl-mmap-tests) mvector-char-ops
  (let ((vec (make-instance 'mapped-vector-char
                            :length (length *ascii-characters*))))
    (unwind-protect
         (progn
           (loop
              for i from 0 below (length-of vec)
              do (setf (mref vec i) (char-code (elt *ascii-characters* i))))
           (let ((contents (loop
                              for i from 0 below (length-of vec)
                              collect (code-char (mref vec i)))))
             (ensure (equalp *ascii-characters* contents))))
      (ensure (free-mapped-vector vec)))))

(addtest (cl-mmap-tests) mvector-ushort-ops
  (let ((vec (make-instance 'mapped-vector-ushort
                            :length (1- (expt 2 16)))))
    (unwind-protect
         (progn
           (loop
              for i from 0 below (length-of vec)
              do (setf (mref vec i) i))
           (loop
              for i from 0 below (length-of vec)
              do (ensure (= i (mref vec i))))
           ;; test incf specifically
           (loop
              for i from 0 below (length-of vec)
              do (incf (mref vec i)))
           (loop
              for i from 0 below (length-of vec)
              do (ensure (= (mref vec i) (1+ i)))))
      (ensure (free-mapped-vector vec)))))

(addtest (cl-mmap-tests) touch
  (let ((test-file (merge-pathnames "data/touch_test.txt")))
    (ensure (not (probe-file test-file)))
    (mm::touch test-file)
    (ensure (probe-file test-file))
    (delete-file test-file)))
