;;; Example for cl-readline.
;;;
;;; This file is to be run with
;;; sbcl --script example.lisp
;;;

(load #p"~/quicklisp/setup.lisp")

;;; Load some systems and define a package...

(asdf:load-system :str)
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
  (labels ((select-completions (list)
             (let ((els (remove-if-not (lambda (it)
                                         (str:starts-with? text it))
                                       list)))
               (if (cdr els)
                   (cons (str:prefix els) els)
                   els))))
    (if (zerop start)
        (select-completions *verbs*)
        (select-completions *fruits*))))

#+test-readline-example
(assert (= 3 (length (custom-complete "ban" 2 2))))

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

(defun echo (text)
  (format t "you said: ~a~&" text))

(defun run-example ()

  (rl:register-function :complete #'custom-complete)
  (rl:bind-keyseq "\\C-o" #'print-some-text)

  (handler-case
      (do ((i 0 (1+ i))
           (text ""))
          ((or (string= "quit" (string-trim " " text))
               ;; C-d:
               (string= "NIL" text)))
        (setf text
              (rl:readline :prompt (format nil "cl-readline ~a> " i)
                           :add-history t
                           :novelty-check #'novelty-check))
        (echo text))
    (#+sbcl sb-sys:interactive-interrupt
      #+ccl  ccl:interrupt-signal-condition
      #+clisp system::simple-interrupt-condition
      #+ecl ext:interactive-interrupt
      #+allegro excl:interrupt-signal
      () (progn
           (format t "~&Bye!~&")
           (uiop:quit)))
    (error (c) (format t "Woops, an unknown error occured:~&~a~&" c))))
;; addition
(run-example)
