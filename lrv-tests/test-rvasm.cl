
(defpackage "TEST-RVASM"
  (:documentation "Instructions for testing risc-v modules assembly instructions")
  (:use :cl :rvasm )
  (:export #:tcomp #:set-env #:cdvec #:linke
           #:*test-name* #:report-result #:report-combined-result #:combine-results
           #:read-first-baits #:read-first-byte #:read-first-jyte #:read-first-vyte
           #:read-first-zyte
           #:check-vait #:check-jait #:do-vait-test #:do-jait-test

   ))

(in-package :test-rvasm)

(defun tcomp (num &optional (len 0))
  "print two's complement of a number in binary form to 'len' places
   or (+ interger-length 1) places"
  (let* ((ilen (+ (integer-length num) 1))
         (olen (if (> ilen len) ilen len)))
    (format t (concatenate 'string "~" (write-to-string olen) ",'0b")
            ;; use "0" as padding because it is hopefully only needed for positive numbers
            (ldb (byte olen 0) num))))

(defun set-env ()
  (progn
    (defparameter *global-env* (make-instance 'basic-env :address 0))
    (setf *env* *global-env*)
    (setf *max-address* 4000)
    ))

(defun cdvec ()
  (env-code-vector *env*))


(defun linke ()
  (link *env*))

(defvar *test-name* nil)

(defun report-jait-result (result form binary)
  "Report the results of a single test case."
  (format t "~:[FAIL~;pass~] ... ~a: ~a~,10t~16,'0b~%" result *test-name* form binary)
  result)

(defun report-vait-result (result form binary)
  "Report the results of a single test case."
  (format t "~:[FAIL~;pass~] ... ~a: ~a~,10t~32,'0b~%" result *test-name* form binary)
  result)

(defun report-combined-result (result )
       (format t "~:[FAIL~;pass~] ... ~a~%" result *test-name*)
  )

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ;; `,(format t "~:[FAIL~;pass~] ... ~a~%" result *test-name*)
       ;; `(report-combined-result ,result)
       ,result)))

(defun read-first-baits (num)
  "read the first 'num' (+ 1) bytes from an environment"
  (coerce (loop for i upto num collect
                               (aref (env-code-vector *env*) i))
          'vector))

(defun read-first-byte ()
    (aref (env-code-vector *env*) 0))

(defun read-first-jyte ()
    (vector
     (aref (env-code-vector *env*) 0 )
     (aref (env-code-vector *env*) 1 )))

(defun read-first-vyte ()
  (read-first-baits 3))

(defun read-first-zyte ()
  (read-first-baits 7))

(defun check-vait (code)
  (equalp (read-first-vyte) (encode-vait code) ))

(defun check-jait (code)
  (equalp (read-first-jyte) (encode-jait code) ))

(defmacro do-vait-test (func test-list)
  "Test the vait output of function with arguments against supplied constant"
  (let ((*test-name* func))
  ;; `(format t "~:[FAIL~;pass~] ... ~a~%"
     ;; `(report-combined-result
            `(combine-results
               ;; `(progn
               ,@(loop for f in test-list collect
                       `(progn ,(set-env)
                               ,(apply func (butlast f 1))
                               ,(report-vait-result (apply #'check-vait (last f))
                                               (butlast f 1) (car (last f)))))
              );  ); ,*test-name*)
    ))

(defun do-vait-test-fun (func test-list)
  (let ((*test-name* func))
  ;; `(format t "~:[FAIL~;pass~] ... ~a~%"
     ;; `(report-combined-result
            (combine-results
               ;; `(progn
               (loop for f in test-list collect
                       (progn (set-env)
                               (apply func (butlast f 1))
                               (report-result (apply #'check-vait (last f))
                                               (butlast f 1) (car (last f)))))
              );  ); ,*test-name*)
    ))

;; (do-vait-test i.or
;;    ((x0  x0  x0    #b00000000000000000110000000010011)
;;     (x31 x0  x0    #b00000000000000000110111110010011)
;;     (x0  x31 x0    #b00000000000011111110000000110011)
;;     (x31 x31 x0    #b00000000000011111110111110110011)
;;     (x0  x0  x31   #b00000001111100000110000000110011)
;;     (x31 x0  x31   #b00000001111100000110111110110011)
;;     (x0  x31 x31   #b00000001111111111110000000110011)
;;     (x31 x31 x31   #b00000001111111111110111110110011)
;;      )
;;   )

(defmacro do-jait-test (func test-list)
  "Test the jait output of function with arguments against supplied constant"
  (let ((*test-name* func))
            `(combine-results
               ,@(loop for f in test-list collect
                       `(progn ,(set-env)
                               ,(apply func (butlast f 1))
                               ,(report-jait-result (apply #'check-jait (last f))
                                               (butlast f 1) (car (last f)))))
              )
    ))







;; (def-vait-test test-or (#'i.or)
;;    ;; '(
;;     '(x0  x0  x0    #b00000000000000000110000000010011)
;;     '(x31 x0  x0    #b00000000000000000110111110010011)
;;     '(x0  x31 x0    #b00000000000011111110000000110011)
;;     '(x31 x31 x0    #b00000000000011111110111110110011)
;;     '(x0  x0  x31   #b00000001111100000110000000110011)
;;     '(x31 x0  x31   #b00000001111100000110111110110011)
;;     '(x0  x31 x31   #b00000001111111111110000000110011)
;;     '(x31 x31 x31   #b00000001111111111110111110110011)

;;     ;; )
;;   )

;; (defmacro def-vait-test (name fun-list test-list)
;;   (let ((func (car funlist))
;;         (forms test-list))
;;     `(defun ,name func forms)
;;        ,@(loop for f in forms collect
;;       `(progn ,(set-env)
;;              ,(apply func (butlast f 1))
;;                     ,(report-result (apply check-vait (last f))
;;                                    (butlast f 1) (last f)))
;;       )
;;      )
;;     )
;; (defmacro def-vait-test (name fun-list &rest test-list)
;;   ;; (let ((func (car funlist))
;;         ;; (forms test-list))
;;     `(defun ,name ()
;;        ,@(loop for f in test-list collect
;;       `(progn ,(set-env)
;;              ,(apply (car fun-list) (butlast f 1))
;;                     ,(report-result (apply #'check-vait (last f))
;;                                    (butlast f 1) (last f)))
;;       )
;;      )
;;   )
