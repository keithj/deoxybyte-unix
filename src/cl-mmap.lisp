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

(in-package :cl-mmap)

(defclass mmapped-file ()
  ((filespec :initform nil
             :initarg :filespec
             :reader filespec-of)
   (delete :initform nil
           :initarg :delete
           :reader delete-of)
   (length :initform nil
           :initarg :length
           :reader length-of)
   (foreign-type :initform :char
                 :initarg :foreign-type
                 :reader foreign-type-of)
   (mmap-length :initarg :mmap-length)
   (mmap-fd :initform nil
            :initarg :mmap-fd)
   (mmap-ptr :initform nil
             :initarg :mmap-ptr)
   (in-memory :initform nil
              :initarg :in-memory)))

(defclass mapped-vector (mmapped-file)
  ())

(defgeneric munmap (mmapped-file)
  (:documentation "Frees the mapped memory used by MMAPPED-FILE and
  closes the underlying file descriptor."))

(defgeneric in-memory-p (mmapped-file)
  (:documentation "Returns T if MMAPPED-FILE is mapped into memory, or
  NIL otherwise."))

(defgeneric mref (mapped-vector index)
  (:documentation "Returns the value at INDEX in MAPPED-VECTOR."))

(defgeneric (setf mref) (value mapped-vector index)
  (:documentation "Sets VALUE at INDEX in MAPPED-VECTOR."))

(defgeneric free-mapped-vector (mapped-vector)
  (:documentation "Frees the mapped memory used by MAPPED-VECTOR."))

(defun mmap (filespec &key (length 0) (foreign-type :char)
             (protection '(:shared)))
  (let ((fd (unix-open filespec '(:rdwr) #o644))
        (flen (* length (foreign-type-size foreign-type)))
        (offset 0))
    (when (= -1 fd)
      (error (unix-strerror *error-number*)))
    (let ((ptr (unix-mmap (null-pointer) flen '(:write) protection fd offset)))
      (when (null-pointer-p ptr)
        (error (unix-strerror *error-number*)))
      (make-instance 'mmapped-file :length length :foreign-type foreign-type
                     :mmap-length flen
                     :mmap-fd (enlarge-file fd (1- flen))
                     :mmap-ptr ptr :in-memory t))))

(defmethod in-memory-p ((obj mmapped-file))
  (with-slots (in-memory)
      obj
    in-memory))

(defmethod munmap ((obj mmapped-file))
  (with-slots (length mmap-length mmap-fd mmap-ptr in-memory)
      obj
    (when in-memory
      (when (= -1 (unix-munmap mmap-ptr mmap-length))
        (error (unix-strerror *error-number*)))
      (when (= -1 (unix-close mmap-fd))
        (error (unix-strerror *error-number*)))
      t)))

(defmethod mref :before ((vector mapped-vector) (index fixnum))
  (with-slots (length)
      vector
    (vector-bounds-check index length)))

(defmethod (setf mref) :before (value (vector mapped-vector) (index fixnum))
  (with-slots (length)
      vector
    (vector-bounds-check index length)))

(defmacro define-mapped-vector (name foreign-type &optional docstring)
  `(progn
     (defclass ,name (mapped-vector)
       ()
       (:documentation ,(or docstring
                            (format nil "A mmapped vector of ~a."
                                    foreign-type))))
     (defmethod initialize-instance :after ((vector ,name) &key)
       (with-slots (filespec length mmap-length foreign-type
                             mmap-fd mmap-ptr in-memory) vector
         (let ((fd (cond (filespec
                          (touch filespec)
                          (unix-open (namestring filespec) '(:rdwr) #o644))
                         (t
                          (make-tmp-fd))))
               (flen (* length (foreign-type-size ,foreign-type)))
               (offset 0))
           (when (= -1 fd)
             (error (unix-strerror *error-number*)))
           (let ((ptr (unix-mmap (null-pointer) flen '(:write) '(:shared)
                                 fd offset)))
             (when (null-pointer-p ptr)
               (error (unix-strerror *error-number*)))
             (setf foreign-type ,foreign-type
                   mmap-length flen
                   mmap-fd (enlarge-file fd (1- flen))
                   mmap-ptr ptr
                   in-memory t)))
         vector))
    (defmethod free-mapped-vector ((vector ,name))
      (prog1
          (munmap vector)
        (with-slots (filespec delete)
            vector
          (when (and filespec delete)
            (delete-file filespec)))))
    (defmethod mref ((vector ,name) (index fixnum))
      (declare (optimize (speed 3) (safety 1)))
      (with-slots (length mmap-ptr) vector
        (mem-aref mmap-ptr ,foreign-type index)))
    (defmethod (setf mref) (value (vector ,name) (index fixnum))
      (declare (optimize (speed 3) (safety 1)))
      (with-slots (length mmap-ptr) vector
        (declare (type fixnum length index))
        (setf (mem-aref mmap-ptr ,foreign-type index) value)))))

(define-mapped-vector mapped-vector-char :char)
(define-mapped-vector mapped-vector-uchar :unsigned-char)
(define-mapped-vector mapped-vector-short :short)
(define-mapped-vector mapped-vector-ushort :unsigned-short)
(define-mapped-vector mapped-vector-int :int)
(define-mapped-vector mapped-vector-uint :unsigned-int)
(define-mapped-vector mapped-vector-float :float)
(define-mapped-vector mapped-vector-double :double)

(defmacro with-mapped-vector ((var class &rest initargs)
                              &body body)
  `(let ((,var (make-instance ,class ,@initargs)))
    (unwind-protect
         (progn
           ,@body)
      (free-mapped-vector ,var))))

(declaim (inline vector-bounds-check))
(defun vector-bounds-check (index length)
  (declare (optimize (speed 3)))
  (declare (type fixnum index length))
  (unless (< -1 index length)
    (error "Index ~a out of bounds: expected a value >= 0 and < ~a."
           index length))
  t)

(defun enlarge-file (fd new-length)
  (when (= -1 (unix-lseek fd new-length :seek-set))
    (error (unix-strerror *error-number*)))
  (when (= -1 (unix-write fd "" 1))
    (error (unix-strerror *error-number*)))
  fd)

(defun make-tmp-fd ()
  (let ((file (unix-tmpfile))
        (fd nil))
    (when (null-pointer-p file)
      (error (unix-strerror *error-number*)))
    (unwind-protect
         (setf fd (unix-fileno file))
      (foreign-free file))
    fd))

(defun touch (filespec)
  (with-open-file (stream filespec :direction :output
                   :if-does-not-exist :create
                   :if-exists nil)))
