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

(defstruct mmap-area
  "An aggregate of data used to describe an mmap operation.

- fr: The mmap file descriptor.
- type: The foreign type in the file being mmapped.
- size: The size in bytes of the region mmapped.
- ptr: The CFFI pointer returned by the mmap foreign function.
- livep: A boolean value which is T if the file is currently mmapped,
  or NIL otherwise."
  (fd 0 :type fixnum)
  (type :char :type symbol)
  (size 0 :type fixnum)
  (ptr nil)
  (live-p nil :type boolean))

(defclass mapped-file ()
  ((filespec :initform nil
             :initarg :filespec
             :reader filespec-of
             :documentation "A user-supplied pathname designator for
             the file to be mmapped, or NIL an automatically generated
             tmp file is to be used.")
   (delete :initform nil
           :initarg :delete
           :reader delete-of
           :documentation "A flag to indicate whether the file
           designated in the FILESPEC slot is to be deleted
           automatically when the mmapped file is freed. If an
           automatically generated tmp is to be used, the value of
           this slot is ignored.")
   (length :initform (error "A length argument is required.")
           :initarg :length
           :reader length-of
           :documentation "The length of the Lisp vector created when
           the file is mmapped.")
   (mmap-area :initform nil
              :initarg :mmap-area
              :documentation "The mmap-area. This slot symbol is
              exported to allow direct access without method
              dispatch. This is significantly faster, at least on
              SBCL.")))

(defclass mapped-vector (mapped-file)
  ()
  (:documentation "A vector backed by a mapped file."))

(defgeneric munmap (mapped-file)
  (:documentation "Frees the mapped memory used by MAPPED-FILE and
  closes the underlying file descriptor."))

(defgeneric in-memory-p (mapped-file)
  (:documentation "Returns T if MAPPED-FILE is mapped into memory, or
  NIL otherwise."))

(defgeneric mref (mapped-vector index)
  (:documentation "Returns the value at INDEX in MAPPED-VECTOR."))

(defgeneric (setf mref) (value mapped-vector index)
  (:documentation "Sets VALUE at INDEX in MAPPED-VECTOR."))

(defgeneric free-mapped-vector (mapped-vector)
  (:documentation "Frees the mapped memory used by MAPPED-VECTOR."))

(defun mmap (filespec &key (length 0) (foreign-type :char)
             (protection '(:shared)))
  "Maps a file into memory.

Arguments:

- filespec (pathname designator): The file to be mmapped.

Key:

- length (fixnum): The length of the Lisp vector created when the file
  is mmapped.
- foreign-type (symbol): The foreign type of the elements to be stored
  in the vector.
- protection (list symbol): The memory protection keyword flags used
  in the mmap operation.

Returns:

- A pointer."
  (let ((fd (c-open (namestring filespec) '(:rdwr) #o644))
        (flen (* length (foreign-type-size foreign-type)))
        (offset 0))
    (when (= -1 fd)
      (error (c-strerror *c-error-number*)))
    (let ((ptr (c-mmap (null-pointer) flen '(:read :write)
                       protection fd offset)))
      (when (null-pointer-p ptr)
        (error (c-strerror *c-error-number*)))
      (make-instance 'mapped-file :length length
                     :mmap-area (make-mmap-area
                                 :fd (enlarge-file fd (1- flen))
                                 :type foreign-type
                                 :size flen
                                 :ptr ptr
                                 :live-p t)))))

(defmethod print-object ((mapped-file mapped-file) stream)
  (with-accessors ((filespec filespec-of) (length length-of))
      mapped-file
    (with-slots ((area mmap-area))
        mapped-file
      (format stream "#<MAPPED-FILE ~@[~a ~]~a ~d elements, ~d bytes>"
              filespec (mmap-area-type area) length (mmap-area-size area)))))

(defmethod print-object ((mapped-vector mapped-vector) stream)
  (with-accessors ((filespec filespec-of) (length length-of))
      mapped-vector
    (with-slots ((area mmap-area))
        mapped-vector
      (format stream "#<MAPPED-VECTOR ~@[~a ~]~a ~d elements, ~d bytes>"
              filespec (mmap-area-type area) length (mmap-area-size area)))))

(defmethod in-memory-p ((obj mapped-file))
  (with-slots ((area mmap-area))
      obj
    (mmap-area-live-p area)))

(defmethod munmap ((obj mapped-file))
  (with-slots ((area mmap-area))
      obj
    (when (mmap-area-live-p area)
      (when (= -1 (c-munmap (mmap-area-ptr area) (mmap-area-size area)))
        (error (c-strerror *c-error-number*)))
      (when (= -1 (c-close (mmap-area-fd area)))
        (error (c-strerror *c-error-number*)))
      (setf (mmap-area-live-p area) nil)
      t)))

(defmethod free-mapped-vector ((vector mapped-vector))
  (prog1
      (munmap vector)
    (with-slots (filespec delete)
        vector
      (when (and filespec delete)
        (delete-file filespec)))))

(defmethod mref :before ((vector mapped-vector) (index fixnum))
  (%vector-bounds-check vector index))

(defmethod (setf mref) :before (value (vector mapped-vector) (index fixnum))
  (declare (ignore value))
  (%vector-bounds-check vector index))

(defmacro define-mapped-vector (name foreign-type &optional docstring)
  "Defines a mapped vector class NAME, with accompanying accessor
methods ( {defmethod mref} ), specialized to store elements of
FOREIGN-TYPE."
  `(progn
     (defclass ,name (mapped-vector)
       ()
       (:documentation ,(or docstring
                            (format nil "A mapped vector of ~a."
                                    foreign-type))))
     (defmethod initialize-instance :after ((vector ,name) &key
                                            (initial-element 0 init-elem-p))
       (with-slots (filespec length mmap-area)
           vector
         (let* ((file-exists (and filespec (probe-file filespec)))
                (fsize (* length (foreign-type-size ,foreign-type)))
                (offset 0)
                (fd (cond (file-exists
                           (c-open (namestring filespec)
                                   '(:rdwr) #o644))
                          (filespec
                           (c-open (namestring
                                    (ensure-file-exists filespec))
                                   '(:rdwr) #o644))
                          (t
                           (c-mkstemp
                            (copy-seq (unix-tmpfile-template)))))))
           (when (= -1 fd)
             (error (c-strerror *c-error-number*)))
           (let ((ptr (c-mmap (null-pointer) fsize '(:read :write)
                              '(:shared) fd offset)))
             (when (null-pointer-p ptr)
               (error (c-strerror *c-error-number*)))
             (unless (and file-exists (not init-elem-p))
               (enlarge-file fd (1- fsize))
               (loop
                  for i from 0 below length
                  do (setf (mem-aref ptr ,foreign-type i) initial-element)))
             (setf mmap-area (make-mmap-area :fd fd
                                             :type ,foreign-type
                                             :size fsize
                                             :ptr ptr
                                             :live-p t)))))
       vector)
    ;; Allow safety 0 for reading
    (defmethod mref ((vector ,name) (index fixnum))
      (declare (optimize (speed 3) (safety 0)))
      (with-slots ((area mmap-area))
          vector
        (mem-aref (mmap-area-ptr area) ,foreign-type index)))
    ;; Use safety 1 to keep the type check on SBCL
    (defmethod (setf mref) (value (vector ,name) (index fixnum))
      (declare (optimize (speed 3) (safety 1)))
      (with-slots ((area mmap-area))
          vector
        (declare (type fixnum index))
        (setf (mem-aref (mmap-area-ptr area) ,foreign-type index) value)))))

(define-mapped-vector mapped-vector-char :char)
(define-mapped-vector mapped-vector-uchar :unsigned-char)

(define-mapped-vector mapped-vector-short :short)
(define-mapped-vector mapped-vector-ushort :unsigned-short)

(define-mapped-vector mapped-vector-int :int)
(define-mapped-vector mapped-vector-uint :unsigned-int)

(define-mapped-vector mapped-vector-float :float)
(define-mapped-vector mapped-vector-double :double)

(define-mapped-vector mapped-vector-int16 :int16)
(define-mapped-vector mapped-vector-uint16 :uint16)

(define-mapped-vector mapped-vector-int32 :int32)
(define-mapped-vector mapped-vector-uint32 :uint32)

(defmacro with-mapped-vector ((var class &rest initargs)
                              &body body)
  "Executes BODY in the context of a newly instantiated
{defclass mapped-vector} object of CLASS bound to VAR. The vector
is safely munmapped after use."
  `(let ((,var (make-instance ,class ,@initargs)))
    (unwind-protect
         (progn
           ,@body)
      (free-mapped-vector ,var))))

(declaim (inline %vector-bounds-check))
(defun %vector-bounds-check (vector index)
  "Performs a bounds check on INDEX with respect to LENGTH. Returns T
if  0 <= INDEX < LENGTH, or raises an error otherwise."
  (declare (optimize (speed 3)))
  (with-slots (length)
      vector
    (declare (type fixnum index length))
    (unless (< -1 index length)
      (error 'mapped-index-error
             :mapped-file vector
             :index index
             :text "index out of bounds")))
  t)

(defun enlarge-file (fd new-length)
  "Enlarges the open file designated by Unix file descriptor FD to
NEW-LENGTH bytes."
  (when (= -1 (c-lseek fd new-length :seek-set))
    (error (c-strerror *c-error-number*)))
  (when (= -1 (c-write fd "" 1))
    (error (c-strerror *c-error-number*)))
  fd)

(defun unix-tmpfile-template ()
  "Returns a new temporary file template string suitable for the C
mkstemp function. The template is merged with
*default-tmpfile-defaults* to supply the directory component."
  (namestring (merge-pathnames (make-pathname :name "deoxybyte-unix-XXXXXX")
                               *default-tmpfile-defaults*)))
