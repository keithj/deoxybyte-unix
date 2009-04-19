;;;
;;; Copyright (C) 2007-2009, Keith James. All rights reserved.
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
  (when (asdf:find-system :cl-system-utilities nil)
    (asdf:operate 'asdf:load-op :cl-system-utilities)))

(defpackage #:cl-mmap-system
  (:use :common-lisp :asdf :cl-system-utilities))

(in-package #:cl-mmap-system)

(defsystem cl-mmap
  :name "Common Lisp mmap"
  :author "Keith James"
  :licence "GPL v3"
  :in-order-to ((test-op (load-op :cl-mmap :cl-mmap-test)))
  :depends-on (:cffi :cl-io-utilities)
  :components ((:module :cl-mmap-core
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

