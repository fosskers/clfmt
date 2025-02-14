
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

(defun for (f)
  "Reducer: Call some function on every item to be reduced, and yield a final nil."
  (lambda (&optional (acc nil a?) (input nil i?))
    (declare (ignore acc))
    (cond ((and a? i?) (funcall f input))
          ((and a? (not i?)) nil)
          (t nil))))

(defun reformat ()
  "Transducer: The composed logic for reformatting a stream of lines."
  (t:comp (t:map (lambda (line) (string-right-trim +trim+ line)))
          (t:drop-while #'empty?)
          (t:window 2)
          (t:filter (lambda (pair) (not (and (empty? (car pair))
                                             (empty? (cadr pair))))))
          (smart-car)))

(defun clfmt/reformat (reducer source)
  "From some SOURCE that yields lines, read those lines and apply simple reformatting."
  (t:transduce (reformat) reducer source))

#++
(clfmt/reformat #'t:cons #p"src/main.lisp")

#++
(defconstant +args+
  '(("")))

(defun main ()
  (cond ((= 2 (length ext:*command-args*))
         (clfmt/reformat (for (lambda (item) (format t "~a~%" item)))
                         (pathname (nth 1 ext:*command-args*))))
        (t (format t "Try harder!~%")
           (ext:quit 1))))

;; (foo) 1
;; 2
;; 3
;; (bar)
