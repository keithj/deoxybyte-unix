;;;
;;; Copyright (c) 2009-2011 Keith James. All rights reserved.
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

(defpackage :uk.co.deoxybyte-unix-ffi
  (:use #:common-lisp #:cffi)
  (:nicknames #:deoxybyte-unix-ffi #:unix-ffi)
  (:export
   #:off-t
   #:size-t
   #:*c-error-number*
   #:seek-directive
   #:sysconf-arg
   #:c-close
   #:c-fileno
   #:c-lseek
   #:c-mkstemp
   #:c-mmap
   #:c-munmap
   #:c-open
   #:c-strerror
   #:c-sysconf
   #:c-write)
  (:documentation "The deoxybyte-unix-ffi package provides utility
foreign functions to Unix via CFFI. This is a low-level FFI that does
not provide a Lisp-style layer on top of the basic Unix functions. A
small subset of Unix functionality is represented, with further
functions being added as required."))

(defpackage :uk.co.deoxybyte-unix
  (:use #:common-lisp #:cffi
        #:deoxybyte-utilities #:deoxybyte-io #:deoxybyte-unix-ffi)
  (:nicknames
   #:deoxybyte-unix #:dxn)
  (:export
   ;; streams and file descriptors
   #:file-descriptor
   #:maybe-standard-stream

   ;; memory map
   #:mmap
   #:munmap
   #:mapped-file-error
   #:mapped-index-error
   #:mapped-file
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

   #:mmap-area
   #:mmap-area-fd
   #:mmap-area-type
   #:mmap-area-size
   #:mmap-area-ptr
   #:mmap-area-live-p
   #:length

   #:filespec-of
   #:delete-policy-of
   #:length-of
   #:in-memory-p
   #:mref
   #:define-mapped-vector
   #:with-mapped-vector
   #:free-mapped-vector)
  (:documentation "The deoxybyte-unix package provides a Lisp style
interface to the low level FFI in the :deoxybyte-unix-ffi package.

Some, but not all, Lisp implementations provide a POSIX or Unix
package. While deoxybyte-unix treads some well-worn ground in that
respect, it should be portable to all Unix platforms supported by
CFFI."))
