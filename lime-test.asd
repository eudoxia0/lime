(defsystem lime-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:lime
               :external-program
               :alexandria
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "lime")))))
