Introduction

The deoxybyte-unix system provides access to Unix functions via CFFI.

Some, but not all, Lisp implementations provide their own POSIX or
Unix package. While deoxybyte-unix treads some well-worn ground in
that respect, it should be portable to all Unix platforms supported by
CFFI.

Installation

deoxybyte-unix uses ASDF for system definition. Copy or symlink
deoxybyte-unix.asd (and optionally deoxybyte-unix-test.asd) to your
asdf:*central-registry* and load deoxybyte-unix with the asdf:operate
function:

 (asdf:operate 'asdf:load-op :deoxybyte-unix)

or with the equivalent deoxybyte-systems:load-system function:

 (dxs:load-system :deoxybyte-unix)


Tests

To run the unit and regression tests you need to have LIFT
installed. Run the tests with the asdf:operate function:

 (asdf:operate 'asdf:test-op :deoxybyte-unix)

or with the equivalent deoxybyte-systems:test-system function:

 (dxs:test-system :deoxybyte-unix)


Documentation

See the Lisp docstrings, particularly the package docstrings for an
overview. HTML documentation may be generated with the command:

 (dxs:document-system :deoxybyte-unix)

at the REPL, provided that CLDOC is installed.


Dependencies

deoxybyte-systems       git://github.com/keithj/deoxybyte-systems.git
deoxybyte-utilities     git://github.com/keithj/deoxybyte-utilities.git
deoxybyte-io            git://github.com/keithj/deoxybyte-io.git

CFFI                    http://common-lisp.net/project/cffi/


Optional dependencies

LIFT                    http://common-lisp.net/project/lift/
CLDOC                   http://common-lisp.net/project/cldoc/
