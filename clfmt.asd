(defsystem :clfmt
  :version "0.1.0"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "MPL-2.0"
  :homepage "https://github.com/fosskers/clfmt"
  :depends-on (:transducers :filepaths)
  :serial t
  :components ((:module "src" :components ((:file "main")))))
