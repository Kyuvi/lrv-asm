
       ;;;;;;;; register size agnostic base derived risc v functions ;;;;;;;;


(defpackage "RVIDRV"
  (:documentation "Risc-V functions that are register size agnostic and
                   that are derived only from the base instructions ")
  (:nicknames :rdv)
  (:use :cl :rvasm :i-32-rv) ;; ?? not portable!!
  (:shadow not)
  (:export #:nop4 #:seqz #:not #:neg
           #:seqz #:snez #:sltz #:sgtz
           #:bgt #:bgtu #:ble #:bleu ;;#:beqz #:bnez
           #:bgez #:blez #:bltz #:bgtz
           #:call #:tail
           #:inc #:dec
           ;; #:li
   ))


(in-package "RVIDRV")

(defun nop4 ()
  "Advances one cycle and advances *pc* by 4"
  ;; (add 'x0 'x0 'x0))
  (i.addi 'x0 'x0 0))


        ;;;; Integer Computational Instructions ;;;;

(defun seqz (rd rs1)
  "Set rd to one if rs1 is equal to zero (otherwise nil)"
  (i.sltiu rd rs1 0))

(defun not (rd rs1)
 "Performs bitwise logical inversion of rs1 and store the result in rd"
  (i.xori rd rs1 -1))

(defun inc (reg)
  "increment REG by one"
  (addi reg reg 1))

(defun dec (reg)
  "decrement REG by one"
  (addi reg reg -1))


        ;;;; Register only Computational instructions ;;;;

(defun neg (rd rs)
  "Stores the negation of the contents of rs in rd"
  (i.sub rd 'x0 rs))

(defun seqz (rd rs)
 "Set rd to one if rs is not equal to zero"
  (i.sltiu rd rs 1))

(defun snez (rd rs)
 "Set rd to one if rs is not equal to zero"
  (i.sltu rd 'x0 rs))

(defun sltz (rd rs)
 "Set rd to one if rs is not equal to zero"
  (i.slt rd rs 'x0))

(defun sgtz (rd rs)
 "Set rd to one if rs is not equal to zero"
  (i.slt rd 'x0 rs))

        ;;;; Jumps and branches ;;;;

(defun bgt (rs1 rs2 imm12)
  "Branch to address described by immediate if rs1 is greater than rs2"
  (i.blt rs2 rs1 imm12))

(defun bgtu (rs1 rs2 imm12)
  "Branch to address described by immediate if rs1 is greater than rs2
   using unsigned comparison"
  (i.bltu rs2 rs1 imm12))

(defun ble (rs1 rs2 imm12)
  "Branch to address described by immediate if rs1 is less than or equal to rs2"
  (i.bge rs2 rs1 imm12))

(defun bleu (rs1 rs2 imm12)
  "Branch to address described by immediate if rs1 is greater than rs2
   using unsigned comparison"
  (i.bgeu rs2 rs1 imm12))

;; moved out as Compressed instructions can be used
;; (defun beqz (rs1 imm)
;;   "Branch to address described by immediate if rs1 is equal to 0"
;;   (i.beq rs1 'x0 imm))

;; (defun bnez (rs1 imm)
;;   "Branch to address described by immediate if rs1 not equal to 0"
;;   (i.bne rs1 'x0 imm))

(defun bgez (rs1 imm12)
  "Branch to address described by immediate if rs1 greater than or equal to 0"
  (i.bge rs1 'x0 imm12))

(defun blez (rs1 imm12)
  "Branch to address described by immediate if rs1 less than or equal to 0"
  (i.bge 'x0 rs1 imm12))

(defun bltz (rs1 imm12)
  "Branch to address described by immediate if rs1 not less than 0"
  (i.blt rs1 'x0 imm12))

(defun bgtz (rs1 imm12)
  "Branch to address described by immediate if rs1 greater than 0"
  (i.blt 'x0 rs1 imm12))



        ;;;; Combinations ;;;;


;; TODO: is it possible to add comprsessed instructions?
(defun call (imm)
  "Call far away subroutine (pseudoinstruction for auipc x1 and jalr x1)"
  (let ((imm12 (delay :imm12 (imm) (logand imm #x00000fff)))
        (imm20 (delay :imm20 (imm) (logand imm #xfffff000))))
    (i.auipc 'x1
           ;; imm20 )
           (delay :call (imm12 imm20) (if (= (logand imm12 #x800) #x800)
                                        ;; test for addi overflow/sign extension??
                                        (+ imm20 #x1000) imm20 )))
                                        ;;simulate overflow/sign extension
    ;; (i.jalr 'x1 'x1 imm12))
    (emit-vait
      (delay :addli (imm12)
        (build-expr-code '(12 5 3 5 7) imm12 (regno 'x1) 0 (regno 'x1) #x13)))
  ))

;; TODO: is it possible to add comprsessed instructions?
(defun tail (imm)
  "Tail call far away subroutine
   (pseudoinstruction for  (auipc x6) and (jalr x0 x6))"
  (let ((imm12 (delay :imm12 (imm) (logand imm #x00000fff)))
        (imm20 (delay :imm20 (imm) (logand imm #xfffff000))))
    (i.auipc 'x6
           ;; imm20 )
           (delay :tail (imm12 imm20) (if (= (logand imm12 #x800) #x800)
                                        ;; test for addi overflow/sign extension??
                                        (+ imm20 #x1000) imm20 )))
                                        ;;simulate overflow/sign extension
    ;; (i.jalr 'x0 'x6 imm12))
    (emit-vait
      (delay :addli (imm12)
        (build-expr-code '(12 5 3 5 7) imm12 (regno 'x0) 0 (regno 'x6) #x13)))
    )
  )



