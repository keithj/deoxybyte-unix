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

(defpackage #:cl-mmap
  (:use #:common-lisp #:cffi)
  (:nicknames #:mm)
  (:export
   ;; Constants

   ;; Variables

   ;; Macros
   #:define-mapped-vector
   #:with-mapped-vector
   
   ;; Functions
   #:mmap

   ;; Classes
   #:mmapped-file
   #:mapped-vector
   #:mapped-vector-char
   #:mapped-vector-uchar
   #:mapped-vector-short
   #:mapped-vector-ushort
   #:mapped-vector-int
   #:mapped-vector-uint
   #:mapped-vector-float
   #:mapped-vector-double

   ;; Generic functions
   #:filespec-of
   #:length-of
   #:in-memory-p
   #:munmap

   #:mref
   #:free-mapped-vector
   #:delete-mapped-vector))
