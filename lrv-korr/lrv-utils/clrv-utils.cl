
(defpackage "CLRV-UTILS"
  (:documentation "General common lisp procedures and macros that are useful in a
                   low-level context")
  (:nicknames :clrv)
  (:use :cl :rvasm)
  (:export
          #:export-mutiple-constants
   ))


(defmacro export-mutiple-constants (&rest constant-lists)
  "(defconsts (&rest constant-lists))
   Define multiple constants and export them"
  (let ((name-list (loop for i in constant-lists collect (car i))))
    `(progn
       ,@(loop for i in constant-lists collect
               `(defconstant ,@i))
      (export ',name-list)))
       )
      ;; (export '(name-list))))
      ;; `(export ',name-list)))
  ;; )

;; (export-mutiple-constants (test #x4 "comment") (v 2))


;; (defmacro def-export-mutiple-constants (&rest constant-lists)
;;   "(defconsts (&rest constant-lists))
;;    Define multiple constants and export them"
;;   `(progn
;;      ,@(loop for i in constant-lists collect
;;              `(defconstant ,@i)
;;              )
;;      ,@(loop for i in constant-lists collect
;;              `(export  (list (car ',i)))
;;              ))
;;   )

;; ;; (def-export-mutiple-constants (test #x4) (v 2))

;; (defmacro defconsts (&rest constant-lists)
;;   "(defconsts (&rest constant-lists))
;;    Define multiple constants."
;;   `(progn
;;      ,@(loop for i in constant-lists collect
;;              `(defconstant ,@i)))
;;   )

;; ;; (defconsts (test1 #x4))

;; (defun export-defconsts (&rest constant-lists)
;;   (let ((name-list (loop for i in constant-lists collect (car i))))
;;     (apply #'defconsts constant-lists)
;;     ;; (export name-list)
;;     ;; name-list)
;;     )
;;   )



;; (export-defconsts '(test #x4) '(v 2))
