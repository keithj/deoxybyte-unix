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

(in-package :uk.co.deoxybyte-unix)

(defun maybe-standard-stream (designator)
  "Returns a standard stream (*standard-input* *standard-output* or
*error-output*) if DESIGNATOR is a string that is STRING-EQUAL to one
of \"stdin\", \"stdout\" or \"stderr\", otherwise returns
DESIGNATOR. (Also works for \"/dev/stdin\" etc.) This function is
useful where one of these strings may be given on a command line to
indicate a system stream, rather than a file-stream is to be used."
  (etypecase designator
    (stream designator)
    (pathname designator)
    (string (cond ((or (string-equal "/dev/stdin" designator)
                       (string-equal "stdin" designator))
                   *standard-input*)
                  ((or (string-equal "/dev/stdout" designator)
                       (string-equal "stdout" designator))
                   *standard-output*)
                  ((or (string-equal "/dev/stderr" designator)
                       (string-equal "stderr" designator))
                   *error-output*)
                  (t
                   designator)))))
