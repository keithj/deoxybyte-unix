;;;
;;; Copyright (c) 2013 Keith James. All rights reserved.
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

(defun file-descriptor (stream &optional direction)
  "Returns the Unix file descriptor associated with STREAM."
  (cond (direction
         (ccl:stream-device stream direction))
        ((and (input-stream-p stream) (not (output-stream-p stream)))
         (ccl:stream-device stream :input))
        ((and (output-stream-p stream) (not (input-stream-p stream)))
         (ccl:stream-device stream :output))
        (t
         (check-arguments nil (direction) "no direction was specified"))))

