;;;
;;; Copyright (c) 2009-2013 Keith James. All rights reserved.
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

(defpackage :uk.co.deoxybyte-unix-test
  (:use #:common-lisp #:deoxybyte-unix #:lift)
  (:documentation "Deoxybyte Unix tests.")
  (:import-from #:deoxybyte-utilities #:invalid-argument-error)
  (:import-from #:deoxybyte-io #:with-tmp-pathname)
  (:export #:deoxybyte-unix-tests))
