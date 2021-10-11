


(defpackage "RV-PRIVILEGED"
  (:documentation "Risc-V instructions for the Privileged Architecture")
  (:nicknames :prv :rvpv)
  (:use :cl :rvasm)
  (:export #:uret #:sret #:mret
           #:wfi
           #:sfence.vma
   ))




;; (defun ecall ()
;;   "(i.ecall)"
;;   (emit-vait (build-expr-code '(16 16) 0 #x73)))

;; (defun ebreak ()
;;   "(i.ebreak)"
;;   (emit-vait (build-expr-code '(16 16) #x10 #x73))) ;c

(defun uret ()
  "(uret)"
  (emit-vait (build-expr-code '(16 16) #x20 #x73)))

(defun sret ()
  "(sret)"
  (emit-vait (build-expr-code '(16 16) #x1020 #x73)))

(defun mret ()
  "(mret)
   Return to Machine mode from trap. "
  (emit-vait (build-expr-code '(16 16) #x3020 #x73)))

(defun wfi ()
  "(wfi)
   Wait for interrupt"
  (emit-vait (build-expr-code '(16 16) #x1050 #x73)))

(defun sfence.vma (rs1 rs2)
  "(sfence.vma)"
  (emit-vait (build-expr-code '(7 5 5 3 12) #x9 rs2 rs1 #x0 #x73)))

(defun ebreak ()
  "(i.ebreak)"
  (emit-vait (build-expr-code '(16 16) #x10 #x73))) ;c
