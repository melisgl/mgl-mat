;;;; -*- mode: Lisp -*-

(asdf:defsystem mgl-mat-test
  :depends-on (#:mgl-mat #:cl-fad)
  :components ((:module "test"
                :serial t
                :components ((:file "test-mat")))))
