(defsystem lime-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:lime
               :fiveam
               :inferior-lisp)
  :components ((:module "t"
                :serial t
                :components
                ((:file "lime")))))
