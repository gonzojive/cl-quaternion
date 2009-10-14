(defpackage org.iodb.cl-quaternion-system
  (:use :common-lisp :asdf))
(in-package :org.iodb.cl-quaternion-system)

(defsystem cl-quaternion
  :description "."
  :version "0.2.1"
  :author "Red Daly <reddaly at gmail>"
  :license "MIT/X11 License.  See LICENSE file"
  :components ((:module "src"
                        :components
			((:file "quaternion")))))

#+nil
(defsystem cl-quaternion-tests
  :name "cl-quaternion-tests"
  :author "Red Daly <reddaly@gmail.com>"
  :version "0.0.1"
  :licence "MIT"
  :description "CL-QUATERNION test suite."
  :components ((:static-file "cl-quaternion.asd")
               (:module "test"
                        :components
			((:file "package")
			 (:file "all-tests" :depends-on ("package"))
			 (:parenscript-file "cl-quaternion-test" :depends-on ("package")))))
  :depends-on ("stefil" "cl-quaternion" "paren-test" "paren-util"))
