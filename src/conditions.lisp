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

(in-package :uk.co.deoxybyte-unix)

(define-condition mapped-file-error (error)
  ((text :initform nil
         :initarg :text
         :reader text-of
         :documentation "Error message text.")
   (mapped-file :initarg :mmapped-file
                :reader mapped-file-of
                :documentation "The mmapped file where the error occurred."))
  (:report (lambda (condition stream)
             (format stream "mmap error in ~a~@[: ~a~]"
                     (mapped-file-of condition) (text-of condition))))
  (:documentation "An error that is raised during an operation on a
mmapped file."))

(define-condition mapped-index-error (mapped-file-error)
  ((index :initform nil
          :initarg :index
          :reader index-of
          :documentation "The index that caused the error."))
  (:report (lambda (condition stream)
             (format stream "mmap error in ~a~@[: ~a~]"
                     (mapped-file-of condition) (text-of condition))))
  (:documentation "An error that is raised during an index operation
on a mmapped file."))
