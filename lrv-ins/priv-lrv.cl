


(defpackage "RV-PRIVILEGED"
  (:documentation "Risc-V instructions for the Privileged Architecture")
  (:nicknames :prv :rvpv)
  (:use :cl :rvasm)
  (:export #:uret #:sret #:mret
           #:wfi
           #:sfence.vma
   ))

(in-package :rv-privileged)


;; (defun ecall ()
;;   "(i.ecall)"
;;   (emit-vait (build-expr-code '(16 16) 0 #x73)))

;; (defun ebreak ()
;;   "(i.ebreak)"
;;   (emit-vait (build-expr-code '(16 16) #x10 #x73))) ;c

(defun uret ()
  "(uret)
   Return to user mode from trap.
   Sets the pointer counter to the value stored in the uepc register."
  (emit-vait (build-expr-code '(16 16) #x20 #x73)))

(defun sret ()
  "(sret)
   Return to Supervisor mode from trap.
   Sets the pointer counter to the value stored in the sepc register."
  (emit-vait (build-expr-code '(16 16) #x1020 #x73)))

(defun mret ()
  "(mret)
   Return to Machine mode from trap.
   Sets the pointer counter to the value stored in the mepc register."
  (emit-vait (build-expr-code '(16 16) #x3020 #x73)))

(defun wfi ()
  "(wfi)
   Wait for interrupt.
   Provides a hint to the implementation that the current hart can be stalled
   until an interrupt might need servicing"
  (emit-vait (build-expr-code '(16 16) #x1050 #x73)))

(defun sfence.vma (rs1 rs2)
  "(sfence.vma)"
  (emit-vait (build-expr-code '(7 5 5 3 12) #x9 rs2 rs1 #x0 #x73)))

(defun ebreak ()
  "(i.ebreak)"
  (emit-vait (build-expr-code '(16 16) #x10 #x73))) ;c
