;;;
;;; Copyright (c) 2009-2012 Keith James. All rights reserved.
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

(in-package :cl-user)

(asdf:load-system :deoxybyte-systems)

(in-package :uk.co.deoxybyte-systems)

(defsystem deoxybyte-unix
  :name "deoxybyte-unix"
  :version "0.8.0"
  :author "Keith James"
  :licence "GPL v3"
  :in-order-to ((test-op (load-op :deoxybyte-unix :deoxybyte-unix-test))
                (doc-op (load-op :deoxybyte-unix :cldoc)))
  :depends-on ((:version :deoxybyte-systems "1.0.0")
               :cffi                    ; (:version :cffi "0.10.3")
               (:version :deoxybyte-io "0.15.0"))
  :components ((:module :deoxybyte-unix
                        :serial t
                        :pathname "src/"
                        :components ((:file "package")
                                     (:file "deoxybyte-unix-ffi")
                                     (:file "conditions")
                                     (:file "deoxybyte-unix")
                                     (:file "memory-map")
                                     #+:sbcl (:file "sbcl")
                                     #+:ccl (:file "ccl")
                                     #-(or :sbcl :ccl) (:file "default"))))
  :perform (test-op :after (op c)
                    (maybe-run-lift-tests :deoxybyte-unix
                                          "deoxybyte-unix-test.config"))
  :perform (doc-op :after (op c)
                   (maybe-build-cldoc-docs :deoxybyte-unix "doc/html")))
