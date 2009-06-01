;;;
;;; Copyright (C) 2009 Keith James. All rights reserved.
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

(defpackage #:uk.co.deoxybyte-unix-ffi
  (:use #:common-lisp #:cffi)
  (:nicknames #:deoxybyte-unix-ffi
              #:unix-ffi)
  (:export
   #:c-close
   #:c-fileno
   #:c-lseek
   #:c-mkstemp
   #:c-mmap
   #:c-munmap
   #:c-open
   #:c-strerror
   #:c-write)
  (:documentation "The deoxybyte-unix-ffi package provides utility
  foreign functions to Unix via CFFI. This is a low-level FFI that
  does not provide a Lisp-style layer on top of the basic Unix
  functions."))

(defpackage #:uk.co.deoxybyte-unix
  (:use #:common-lisp #:cffi #:deoxybyte-io #:deoxybyte-unix-ffi)
  (:nicknames
   #:deoxybyte-unix
   #:dxx)
  (:export
   ;; Specials
   *error-number*

   ;; Conditions
   #:mmapped-file-error
   #:mmapped-index-error

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

   #:mapped-vector-int32
   #:mapped-vector-uint32

   #:mapped-vector-int16
   #:mapped-vector-uint16

   ;; Generic functions
   #:filespec-of
   #:delete-of
   #:length-of
   #:in-memory-p
   #:munmap

   #:mref
   #:free-mapped-vector)
  (:documentation "The deoxybyte-unix package provides a Lisp style
  interface to the low level FFI in the :deoxybyte-unix-ffi package."))
