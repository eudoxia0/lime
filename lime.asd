(defsystem lime
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :homepage ""
  :bug-tracker ""
  :source-control (:git "")
  :depends-on (:swank-protocol
               :alexandria
               :trivial-types)
  :components ((:module "src"
                :serial t
                :components
                ((:file "lime"))))
  :description "A high-level Swank client, like Slime, but for Common Lisp applications."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op lime-test))))
