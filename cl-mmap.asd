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

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-system :cl-system-utilities nil)
    (asdf:operate 'asdf:load-op :cl-system-utilities)))

(defpackage #:cl-mmap-system
  (:use :common-lisp :cl-system-utilities))

(in-package #:cl-mmap-system)

(defsystem cl-mmap
    :name "Common Lisp mmap"
    :author "Keith James"
    :version "0.2.0"
    :licence "GPL v3"
    :depends-on (:cffi)
    :in-order-to ((test-op (load-op :cl-mmap :cl-mmap-test)))
    :components ((:module :cl-mmap
                          :serial t
                          :pathname "src/"

                          :components ((:file "package")
                                       (:file "mmap-cffi")
                                       (:file "cl-mmap")))
                 (:lift-test-config :lift-tests
                                    :pathname "cl-mmap-test.config"
                                    :target-system :cl-mmap)
                 (:cldoc-config :cldoc-documentation
                                :pathname "doc/html"
                                :target-system :cl-mmap)))

