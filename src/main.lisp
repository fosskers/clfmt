
(defpackage clfmt
  (:use :cl)
  (:local-nicknames (#:t #:transducers))
  (:export #:main)
  (:documentation "Lightly reformat your Common Lisp code."))

(in-package :clfmt)

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

(defun work (path)
  "From a PATH to a Common Lisp file, read its contents and apply reformatting."
  (t:transduce (t:comp (t:map (lambda (line) (string-right-trim +trim+ line)))
                       (t:drop-while #'empty?)
                       (t:window 2)
                       (t:filter (lambda (pair) (not (and (empty? (car pair))
                                                          (empty? (cadr pair))))))
                       (smart-car))
               #'t:cons path))

#++
(work #p"src/main.lisp")

(defun main ())





;; (foo) 1
;; 2
;; 3
;; (bar)
