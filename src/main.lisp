(defpackage clfmt
  (:use :cl)
  (:local-nicknames (#:t #:transducers)
                    (#:p #:filepaths))
  (:export #:main)
  (:documentation "Lightly reformat your Common Lisp code."))

(in-package :clfmt)

#-ecl
(error "CLFMT can only be compiled with ECL.")

(defconstant +trim+ #(#\space #\newline))

(declaim (ftype (function (string) boolean) empty?))
(defun empty? (seq)
  (zerop (length seq)))

(defun reformat ()
  "Transducer: The composed logic for reformatting a stream of lines."
  (t:comp (t:map (lambda (line) (string-right-trim +trim+ line)))
          (t:drop-while #'empty?)
          (t:group-by #'empty?)
          (t:map (lambda (list) (cond ((empty? (car list)) '(""))
                                      (t list))))
          #'t:concatenate))

(defun clfmt/reformat (reducer source)
  "From some SOURCE that yields lines, read those lines and apply simple reformatting."
  (t:transduce (reformat) reducer source))

#++
(clfmt/reformat #'t:cons #p"test.txt")

(defun temp-file (orig)
  "Generate a temporary file name based on some original."
  (p:add-extension orig "CLFMT"))

#++
(temp-file "/home/colin/code/foo.lisp")

(defun clfmt/in-place (path)
  "Reformat a file in-place, overwriting the original."
  (labels ((recurse (paths)
             (unless (null paths)
               (let* ((orig (car paths))
                      (temp (temp-file orig)))
                 (with-open-file (stream temp
                                         :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create)
                   (clfmt/reformat (t:for (lambda (line) (format stream "~a~%" line))) orig)
                   (rename-file temp orig :if-exists :supersede)
                   (recurse (cdr paths)))))))
    (let ((checked (probe-file path)))
      (cond ((null checked)
             (format t "Path not found.~%")
             (ext:quit 1))
            ((p:directory? checked)
             (recurse (directory (p:join checked "*.lisp"))))
            (t (recurse (list checked)))))))

(defparameter +help+
  "clfmt - Lightly reformat your Common Lisp code

Usage:
  clfmt <PATH> - Print reformatted Lisp code of a given file to STDOUT
  clfmt        - Accept input from STDIN

Flags:
  -i, --inplace - Overwrite the input file
  --help        - Display this help message
  --version     - Display the current version of clfmt")

(defparameter +args+
  '((("--help" "-h") 0 (princ +help+))
    ("--version" 0 (format t "0.1.1~%"))
    (("--inplace" "-i") 1 (clfmt/in-place 1))
    ("*DEFAULT*" 1 (clfmt/reformat
                    (t:for (lambda (item) (format t "~a~%" item)))
                    (pathname (car 1))) :stop)))

(defun main ()
  (let ((ext:*lisp-init-file-list* nil)
        (ext:*help-message* +help+))
    (cond ((= 1 (length ext:*command-args*))
           (clfmt/reformat (t:for (lambda (item) (format t "~a~%" item))) *standard-input*))
          (t (ext:process-command-args :rules +args+)))
    (ext:quit 0)))
