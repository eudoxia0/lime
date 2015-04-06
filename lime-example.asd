(defsystem lime-example
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on (:lime
               :bordeaux-threads)
  :components ((:module "example"
                :components
                ((:file "repl"))))
  :description "An example REPL built on Lime.")
