;;;
;;; Copyright (C) 2009-2010 Keith James. All rights reserved.
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (asdf:find-system :deoxybyte-systems nil)
    (asdf:operate 'asdf:load-op :deoxybyte-systems)))

(defpackage :uk.co.deoxybyte-unix-system
  (:use :common-lisp :asdf)
  (:import-from :deoxybyte-systems :lift-test-config :cldoc-config))

(in-package :uk.co.deoxybyte-unix-system)

(defsystem deoxybyte-unix
  :name "deoxybyte-unix"
  :version "0.6.3"
  :author "Keith James"
  :licence "GPL v3"
  :in-order-to ((test-op (load-op :deoxybyte-unix :deoxybyte-unix-test)))
  :depends-on ((:version :cffi "0.10.3")
               (:version :deoxybyte-io "0.5.3"))
  :components ((:module :deoxybyte-unix
                        :serial t
                        :pathname "src/"
                        :components ((:file "package")
                                     (:file "deoxybyte-unix-ffi")
                                     (:file "conditions")
                                     (:file "deoxybyte-unix")))
               (:lift-test-config :lift-tests
                                  :pathname "deoxybyte-unix-test.config"
                                  :target-system :deoxybyte-unix)
               (:cldoc-config :cldoc-documentation
                              :pathname "doc/html/"
                              :target-system :deoxybyte-unix)))
