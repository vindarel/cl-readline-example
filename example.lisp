;;; The example from the doc page, untouched.
;;; https://mrkkrp.github.io/cl-readline/#Example
;;;
;;; run with
;;; sbcl --script example.lisp
;;;
;;; Works fine. jan, 2018, Debian, SBCL.

(load #p"~/quicklisp/setup.lisp")

;;; Load some systems and define a package...

(asdf:load-system :alexandria)
(asdf:load-system :cl-readline)

(cl:defpackage :example
  (:use    #:common-lisp
           #:alexandria)
  (:export #:run-example))

(in-package :example)

;;; Now let's define lists of verbs and fruits:

(defvar *verbs*  '("eat" "get" "throw" "quit"))
(defvar *fruits* '("banana" "apple" "orange" "banana_two"))

;;; Define and register function that does custom completion: if user enters
;;; first word, it will be completed as a verb, second and later words will
;;; be completed as fruits.

(defun custom-complete (text start end)
  (declare (ignore end))
  (labels ((common-prefix (items)
             (subseq
              (car items) 0
              (position
               nil
               (mapcar
                (lambda (i)
                  (every (lambda (x)
                           (char= (char (car items) i)
                                  (char x           i)))
                         (cdr items)))
                (iota (reduce #'min (mapcar #'length items)))))))
           (select-completions (list)
             (let ((els (remove-if-not (curry #'starts-with-subseq text)
                                       list)))
               (if (cdr els)
                   (cons (common-prefix els) els)
                   els))))
    (if (zerop start)
        (select-completions *verbs*)
        (select-completions *fruits*))))

(rl:register-function :complete #'custom-complete)

;;; Let's also create a custom command and bind it to some key sequence so
;;; user can invoke it. In this example user can automagically insert phrase
;;; 'inserted text' pressing Control-o.

(defun print-some-text (arg key)
  (declare (ignore arg key))
  (rl:insert-text "inserted text"))

(rl:bind-keyseq "\\C-o" #'print-some-text)

;;; Let's write novelty-check, so if the actual line is equal to the most
;;; recent history line it will not be added to the history.

(defun novelty-check (x y)
  (string/= (string-trim " " x)
            (string-trim " " y)))

;;; Finally, this is our main function. To exit from the loop, enter 'quit'.

(defun run-example ()
  (do ((i 0 (1+ i))
       (text ""))
      ((string= "quit" (string-trim " " text)))
    (setf text
          (rl:readline :prompt (format nil "[~a]> " i)
                       :add-history t
                       :novelty-check #'novelty-check))))

;; addition
(run-example)
