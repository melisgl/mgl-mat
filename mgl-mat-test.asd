;;;; -*- mode: Lisp -*-

(asdf:defsystem mgl-mat-test
  :depends-on (#:mgl-mat)
  :components ((:module "test"
                :serial t
                :components ((:file "test-mat")))))
