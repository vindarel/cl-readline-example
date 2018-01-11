;;; Official cl-readline example.
;;; https://mrkkrp.github.io/cl-readline/

;;; Load some systems and define a package...

(defpackage readline-example
  (:use    #:cl
           #:alexandria)
  (:export #:run-example))

(in-package :readline-example)

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
  (handler-case
      (do ((i 0 (1+ i))
           (text ""))
          ((string= "quit" (string-trim " " text)))
        (setf text
              (rl:readline :prompt (format nil "cl-readline ~a> " i)
                           :add-history t
                           :novelty-check #'novelty-check)))
    (#+sbcl sb-sys:interactive-interrupt
     #+ccl  ccl:interrupt-signal-condition
     #+clisp system::simple-interrupt-condition
     #+ecl ext:interactive-interrupt
     #+allegro excl:interrupt-signal
     () (progn
          ;; (format *error-output* "Aborting.~&")
          (uiop:quit)))
    (error (c) (format t "Woops, an unknown error occured:~&~a~&" c))))
