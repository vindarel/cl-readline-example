(asdf:defsystem readline-example
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on (:cl-readline
               :alexandria)
  :components ((:module "src"
                :components
                ((:file "readline-example"))))
  ;; build
  :build-operation "program-op"
  :build-pathname "clreadline"
  :entry-point "readline-example:run-example"

  ;; :in-order-to ((test-op (test-op "foo")))
  )
