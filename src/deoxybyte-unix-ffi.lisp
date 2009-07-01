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

(in-package :uk.co.deoxybyte-unix-ffi)

(defctype off-t #-x86-64 :uint32
                #+x86-64 :uint64
  "Offset type.")

(defctype size-t #-x86-64 :uint32
                 #+x86-64 :uint64
  "Site type.")

(defcvar ("errno" *c-error-number*) :int
  "Number of last error.")

(defbitfield open-flags
  (:rdonly  #x0000)
  :wronly                               ; #x0001
  :rdwr                                 ; ...
  :nonblock
  :append
  (:creat #x0200))

(defbitfield protection-flags
  (:none #x0000)
  :read
  :write
  :exec)

(defbitfield map-flags
  (:shared #x0001)
   :private
  (:fixed #x0010))

(defcenum seek-directive 
  :seek-set
  :seek-cur
  :seek-end)

(defcfun ("close" c-close) :int
  (file-descriptor :int))

(defcfun ("fileno" c-fileno) :int
  (stream :pointer))

(defcfun ("lseek" c-lseek) :int
  (file-descriptor :int)
  (offset off-t)
  (whence seek-directive))

(defcfun ("mkstemp" c-mkstemp) :int
  (template :string))

(defcfun ("mmap" c-mmap) :pointer
  (address :pointer)
  (length size-t)
  (protection protection-flags)
  (flags map-flags)
  (file-descriptor :int)
  (offset off-t))

(defcfun ("munmap" c-munmap) :int
  (address :pointer)
  (length size-t))

(defcfun ("open" c-open) :int
  (path :string)
  (flags open-flags)
  (mode :unsigned-int))

(defcfun ("strerror" c-strerror) :string
  (errno :int))

(defcfun ("write" c-write) :int
  (file-descriptor :int)
  (value :string)
  (count size-t))
