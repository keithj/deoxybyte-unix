;;;
;;; Copyright (c) 2009-2013 Keith James. All rights reserved.
;;;
;;; This file is part of deoxybyte-unix.
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of thcoe GNU General Public License as published by
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

(in-package :uk.co.deoxybyte-unix-ffi)

(defctype off-t #-x86-64 :uint32
                #+x86-64 :uint64
  "Offset type.")

(defctype size-t #-x86-64 :uint32
                 #+x86-64 :uint64
  "Site type.")

(defcvar ("errno" *c-error-number*) :int
  "Number of last error.")

(defbitfield open-flags
  (:rdonly  #x0000)
  :wronly                               ; #x0001
  :rdwr                                 ; ...
  :nonblock
  :append
  (:creat #x0200))

(defbitfield protection-flags
  (:none #x0000)
  :read
  :write
  :exec)

(defbitfield map-flags
  (:shared #x0001)
   :private
  (:fixed #x0010))

(defcenum seek-directive 
  :seek-set
  :seek-cur
  :seek-end)

(defcenum sysconf-arg
  :sc-arg-max
  :sc-child-max
  :sc-clk-tck
  :sc-ngroups-max
  :sc-open-max
  :sc-stream-max
  :sc-tzname-max
  :sc-job-control
  :sc-saved-ids
  :sc-realtime-signals
  :sc-priority-scheduling
  :sc-timers
  :sc-asynchronous-io
  :sc-prioritized-io
  :sc-synchronized-io
  :sc-fsync
  :sc-mapped-files
  :sc-memlock
  :sc-memlock-range
  :sc-memory-protection
  :sc-message-passing
  :sc-semaphores
  :sc-shared-memory-objects
  :sc-aio-listio-max
  :sc-aio-max
  :sc-aio-prio-delta-max
  :sc-delaytimer-max
  :sc-mq-open-max
  :sc-mq-prio-max
  :sc-version
  :sc-pagesize
  :sc-rtsig-max
  :sc-sem-nsems-max
  :sc-sem-value-max
  :sc-sigqueue-max
  :sc-timer-max

  ;; POSIX2
  :sc-bc-base-max
  :sc-bc-dim-max
  :sc-bc-scale-max
  :sc-bc-string-max
  :sc-coll-weights-max
  :sc-equiv-class-max
  :sc-expr-nest-max
  :sc-line-max
  :sc-re-dup-max
  :sc-charclass-name-max

  :sc-2-version
  :sc-2-c-bind
  :sc-2-c-dev
  :sc-2-fort-dev
  :sc-2-fort-run
  :sc-2-sw-dev
  :sc-2-localedef

  :sc-pii
  :sc-pii-xti
  :sc-pii-socket
  :sc-pii-internet
  :sc-pii-osi
  :sc-poll
  :sc-select
  :sc-uio-maxiov
  :sc-pii-internet-stream
  :sc-pii-internet-dgram
  :sc-pii-osi-cots
  :sc-pii-osi-clts
  :sc-pii-osi-m
  :sc-t-iov-max

  ;; POSIX threads
  :sc-threads
  :sc-thread-safe-functions
  :sc-getgr-r-size-max
  :sc-getpw-r-size-max
  :sc-login-name-max
  :sc-tty-name-max
  :sc-thread-destructor-iterations
  :sc-thread-keys-max
  :sc-thread-stack-min
  :sc-thread-threads-max
  :sc-thread-attr-stackaddr
  :sc-thread-attr-stacksize
  :sc-thread-priority-scheduling
  :sc-thread-prio-inherit
  :sc-thread-prio-protect
  :sc-thread-process-shared
  :sc-nprocessors-conf
  :sc-nprocessors-onln)

(defcfun ("close" c-close) :int
  (file-descriptor :int))

(defcfun ("fileno" c-fileno) :int
  (stream :pointer))

(defcfun ("lseek" c-lseek) :int
  (file-descriptor :int)
  (offset off-t)
  (whence seek-directive))

(defcfun ("mkstemp" c-mkstemp) :int
  (template :pointer))

(defcfun ("mmap" c-mmap) :pointer
  (address :pointer)
  (length size-t)
  (protection protection-flags)
  (flags map-flags)
  (file-descriptor :int)
  (offset off-t))

(defcfun ("munmap" c-munmap) :int
  (address :pointer)
  (length size-t))

(defcfun ("open" c-open) :int
  (path :string)
  (flags open-flags)
  (mode :unsigned-int))

(defcfun ("strerror" c-strerror) :string
  (errno :int))

(defcfun ("sysconf" c-sysconf) :long
  (name sysconf-arg))

(defcfun ("write" c-write) :int
  (file-descriptor :int)
  (value :string)
  (count size-t))
