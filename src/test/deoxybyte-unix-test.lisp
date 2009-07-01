;;;
;;; Copyright (C) 2009 Keith James. All rights reserved.
;;;
;;; This file is part of deoxybyte-unix.
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

(in-package :uk.co.deoxybyte-unix-test)

(deftestsuite deoxybyte-unix-tests ()
  ())

(defparameter *ascii-characters* (loop
                                    for i from 32 to 126
                                    collect (code-char i)))

(addtest (deoxybyte-unix-tests) mvector-char-ops/1
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

(addtest (deoxybyte-unix-tests) mvector-ushort-ops/1
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

(addtest (deoxybyte-unix-tests) mvector-initial-element/1
  (let ((vec1 (make-instance 'mapped-vector-ushort :length 100))
        (vec2 (make-instance 'mapped-vector-ushort :length 100
                             :initial-element 1)))
    (unwind-protect
         (progn
           (loop
              for i from 0 below (length-of vec1)
              do (ensure (zerop (mref vec1 i))))
           (loop
              for i from 0 below (length-of vec2)
              do (ensure (= 1 (mref vec2 i)))))
      (ensure (free-mapped-vector vec1))
      (ensure (free-mapped-vector vec2)))))

(addtest (deoxybyte-unix-tests) mvector-bounds-check/1
   (let ((vec (make-instance 'mapped-vector-ushort :length 100)))
     (unwind-protect
          (progn
            (ensure (zerop (mref vec 0)))
            (ensure (zerop (mref vec 99)))
            (ensure-error
              (mref vec -1))
            (ensure-error
              (mref vec 100))
            (ensure-error
              (setf (mref vec -1) 99))
            (ensure-error
              (setf (mref vec 100) 99)))
       (ensure (free-mapped-vector vec)))))

(addtest (deoxybyte-unix-tests) mmapped-index-error/1
  (let ((vec (make-instance 'mapped-vector-ushort :length 100)))
    (unwind-protect
         (progn
           (ensure (zerop (mref vec 0)))
           (ensure (zerop (mref vec 99)))
           (ensure-condition mmapped-file-error
             (mref vec -1))
           (ensure-condition mmapped-index-error
             (mref vec 100)))
      (ensure (free-mapped-vector vec)))))
