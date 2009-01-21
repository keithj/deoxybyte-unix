;;;
;;; Copyright (C) 2007-2009 Keith James. All rights reserved.
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

(in-package :cl-mmap)

(defctype off-t #-x86-64 :uint32
                #+x86-64 :uint64
  "File offset type for seeking within a mmapped file.")

(defctype size-t #-x86-64 :uint32
                 #+x86-64 :uint64
  "File size for mmapped files.")

(defcvar ("errno" *error-number*) :int)

(defbitfield open-flags
  (:rdonly  #x0000)
   :wronly ; #x0001
   :rdwr   ; ...
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

(defcfun ("strerror" unix-strerror) :string
  (errno :int))

(defcfun ("tmpfile" unix-tmpfile) :pointer)

(defcfun ("fileno" unix-fileno) :int
  (stream :pointer))

(defcfun ("open" unix-open) :int
  (path :string)
  (flags open-flags)
  (mode :unsigned-int))

(defcfun ("close" unix-close) :int
  (file-descriptor :int))

(defcfun ("lseek" unix-lseek) :int
  (file-descriptor :int)
  (offset off-t)
  (whence seek-directive)) 

(defcfun ("write" unix-write) :int
  (file-descriptor :int)
  (value :string)
  (count size-t))

(defcfun ("mmap" unix-mmap) :pointer
  (address :pointer)
  (length size-t)
  (protection protection-flags)
  (flags map-flags)
  (file-descriptor :int)
  (offset off-t))

(defcfun ("munmap" unix-munmap) :int
  (address :pointer)
  (length size-t))
