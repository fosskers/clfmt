(defsystem "transducers"
  :version "1.4.0"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "MPL-2.0"
  :depends-on ()
  :serial t
  :components ((:module "transducers"
                :components
                ((:file "package")
                 (:file "deprecated")
                 (:file "utils")
                 (:file "transducers")
                 (:file "reducers")
                 (:file "sources")
                 (:file "entry")
                 (:file "conditions"))))
  :description "Ergonomic, efficient data processing.")
