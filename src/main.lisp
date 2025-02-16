(defpackage clfmt
  (:use :cl)
  (:local-nicknames (#:t #:transducers))
  (:export #:main)
  (:documentation "Lightly reformat your Common Lisp code."))

(in-package :clfmt)

#-ecl
(error "CLFMT can only be compiled with ECL.")

(defconstant +trim+ #(#\space #\newline))

(defun empty? (seq)
  (zerop (length seq)))

(defun smart-car ()
  "Transducer: Act like `(map #'car)', except that the final `cadr' of the final
pair is also passed through."
  (let ((last nil))
    (lambda (reducer)
      (lambda (result &optional (input nil i-p))
        (if i-p
            (let ((res (funcall reducer result (car input))))
              (setf last (cadr input))
              res)
            (let ((res (funcall reducer result last)))
              (funcall reducer res)))))))

(defun reformat ()
  "Transducer: The composed logic for reformatting a stream of lines."
  (t:comp (t:map (lambda (line) (string-right-trim +trim+ line)))
          (t:drop-while #'empty?)
          (t:window 2)
          (t:filter (lambda (pair) (not (and (empty? (car pair)) (empty? (cadr pair))))))
          (smart-car)))

(defun clfmt/reformat (reducer source)
  "From some SOURCE that yields lines, read those lines and apply simple reformatting."
  (t:transduce (reformat) reducer source))

#++
(clfmt/reformat #'t:cons #p"test.txt")

(defun clfmt/in-place (path)
  "Reformat a file in-place, overwriting the original."
  (format t "~a~%" path))

(defconstant +help+
  "clfmt - Lightly reformat your Common Lisp code

Usage:
  clfmt <PATH> - Print reformatted Lisp code of a given file to STDOUT
  clfmt        - Accept input from STDIN

Flags:
  -i, --inplace - Overwrite the input file
  --help        - Display this help message
  --version     - Display the current version of clfmt")

(defconstant +args+
  '((("--help" "-h") 0 (princ +help+))
    ("--version" 0 (format t "0.1.0~%"))
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
