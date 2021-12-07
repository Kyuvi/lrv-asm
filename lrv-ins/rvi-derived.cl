
       ;;;;;;;; register size agnostic base derived risc v functions ;;;;;;;;


(defpackage "RVIDRV"
  (:documentation "Risc-V functions that are register size agnostic and
                   that are derived only from the base instructions ")
  (:nicknames :rdv)
  (:use :cl :rvreg :rvasm :i-32-rv) ;; ?? not portable!!
  (:shadow not)
  (:export #:nop4 #:seqz #:inc #:dec #:not #:neg
           #:seqz #:snez #:sltz #:sgtz
           #:bgt #:bgtu #:ble #:bleu #:beqz #:bnez
           #:bgez #:blez #:bltz #:bgtz
           #:la
           #:lfb #:lfj #:lfv
           #:sfb #:sfj #:sfv
           #:call #:tail
           ;; #:li
   ))


(in-package "RVIDRV")

;; TODO: Make portable
;; (if (= *base-register-size* 32)
;;     (use :i-32-rv)
;;     (use :i-64-rv))


(defun nop4 ()
 "(nop4)
  Advances one cycle and advances *pc* by 4"
  ;; (add 'x0 'x0 'x0))
  (i.addi 'x0 'x0 0))


        ;;;; Integer Computational Instructions ;;;;

(defun seqz (rd rs1)
 "(seqz rd rs1)
  Set rd to one if rs1 is equal to zero (otherwise nil)"
  (i.sltiu rd rs1 0))

(defun not (rd rs1)
 "(not rd rs1)
  Performs bitwise logical inversion of rs1 and store the result in rd"
  (i.xori rd rs1 -1))

(defun inc (reg &optional (imm12 1))
 "(inc reg &optional (imm12 1))
  increment REG by one or imm12 if given"
  (i.addi reg reg imm12))

(defun binc (reg)
 "(binc reg)
  increment REG by 8"
  (i.addi reg reg 8))

(defun dec (reg &optional (imm12 1))
 "(dec reg &optional (imm12 1))
  decrement REG by by one or imm12 if given"
  (i.addi reg reg (- imm12)))


        ;;;; Register only Computational instructions ;;;;

(defun neg (rd rs)
 "(neg rd rs)
  Stores the negation of the contents of rs in rd"
  (i.sub rd 'x0 rs))

(defun seqz (rd rs)
 "(seqz rd rs)
  Set rd to one if rs is equal to zero"
  (i.sltiu rd rs 1))

(defun snez (rd rs)
 "(snez rd rs)
  Set rd to one if rs is not equal to zero"
  (i.sltu rd 'x0 rs))

(defun sltz (rd rs)
 "(sltz rd rs)
  Set rd to one if rs is less than zero"
  (i.slt rd rs 'x0))

(defun sgtz (rd rs)
 "(sgtz rd rs)
  Set rd to one if rs is greater than zero"
  (i.slt rd 'x0 rs))

        ;;;; Jumps and branches ;;;;

(defun bgt (rs1 rs2 imm12)
 "(bgt rs1 rs2 imm12)
  Branch to address described by immediate if rs1 is greater than rs2"
  (i.blt rs2 rs1 imm12))

(defun bgtu (rs1 rs2 imm12)
 "(bgtu rs1 rs2 imm12)
  Branch to address described by immediate if rs1 is greater than rs2
   using unsigned comparison"
  (i.bltu rs2 rs1 imm12))

(defun ble (rs1 rs2 imm12)
 "(ble rs1 rs2 imm12)
  Branch to address described by immediate if rs1 is less than or equal to rs2"
  (i.bge rs2 rs1 imm12))

(defun bleu (rs1 rs2 imm12)
 "(bleu rs1 rs2 imm12)
  Branch to address described by immediate if rs1 is greater than rs2
   using unsigned comparison"
  (i.bgeu rs2 rs1 imm12))

;; moved out as Compressed instructions can be used
(defun beqz (rs1 imm)
 "(beqz rs1 imm)
  Branch to address described by immediate if rs1 is equal to 0"
  (i.beq rs1 'x0 imm))

(defun bnez (rs1 imm)
 "(bnez rs1 imm)
  Branch to address described by immediate if rs1 not equal to 0"
  (i.bne rs1 'x0 imm))

(defun bgez (rs1 imm12)
 "(bgez rs1 imm12)
  Branch to address described by immediate if rs1 greater than or equal to 0"
  (i.bge rs1 'x0 imm12))

(defun blez (rs1 imm12)
 "(blez rs1 imm12)
  Branch to address described by immediate if rs1 less than or equal to 0"
  (i.bge 'x0 rs1 imm12))

(defun bltz (rs1 imm12)
 "(bltz rs1 imm12)
  Branch to address described by immediate if rs1 not less than 0"
  (i.blt rs1 'x0 imm12))

(defun bgtz (rs1 imm12)
 "(bgtz rs1 imm12)
  Branch to address described by immediate if rs1 greater than 0"
  (i.blt 'x0 rs1 imm12))



        ;;;; Combinations ;;;;

;; (defun li (rd imm))

(defun la (rd imm)
 "(la rd imm)
  Load address: Load (pc relative) address into rv
  assumes use of #h or #y read macros to avoid twos complement errors. "
  (let* ((addr *pc*) (ofst (offset imm))
         (imm12 (delay :imm12 (ofst) (logand ofst #x00000fff)))
         (imm20 (delay :imm20 (ofst) (logand ofst #xfffff000))))
    (emit-vait
     (delay :la (ofst imm12 imm20)
       (if (cl:not (immp ofst 32))
           (rv-error "la: Upper Immediate value out of range." addr)
           (build-expr-code '(20 5 7)
                            (bits (if (= (logand imm12 #x800) #x800)
                                      ;; test for addi overflow/sign extension??
                                      (+ imm20 #x1000) imm20 )
                                  ;;simulate overflow/sign extension
                                  31 12) (regno rd) #x17))))
    ;; (addi crd crd imm12)) ;; does not work because of (immp imm12 12) test
    ;; need to "force" it to accept #x800 as #x-800
    ;; TODO: is it possible to add comprsessed instructions?
    (emit-vait
     (delay :addla (imm12)
       (build-expr-code '(12 5 3 5 7) imm12 (regno rd) 0 (regno rd) #x13)))))

(defun lfb (rd imm)
  ;; (let ((ofst (offset imm)))
  ;;   (if (cl:and (ingtegerp imm) (immp ofst 12))
  ;;       (i.lb rd rd ofst)
  (let* ((addr *pc*)  (ofst (offset imm))
         (imm12 (delay :imm12 (ofst) (logand ofst #x00000fff)))
         (imm20 (delay :imm20 (ofst) (logand ofst #xfffff000))))
    (emit-vait
     (delay :lauipcb (ofst imm12 imm20)
       (if (cl:not (immp ofst 32))
           (rv-error "lfb: Upper Immediate value out of range." addr)
           (build-expr-code '(20 5 7)
                            (bits (if (= (logand imm12 #x800) #x800)
                                      ;; test for addi overflow/sign extension??
                                      (+ imm20 #x1000) imm20 )
                                  ;;simulate overflow/sign extension
                                  31 12) (regno rd) #x17))))
    (emit-vait
     (delay :lfb (imm12)
       (build-expr-code '(12 5 3 5 7) imm12 (regno rd) 0 (regno rd) #x3)))
    ))
                                     ;))

(defun lfj (rd imm)
  ;; (let ((ofst (offset imm)))
  ;;   (if (cl:and (ingtegerp imm) (immp ofst 12))
  ;;       (i.lj rd rd ofst)
  (let* ((addr *pc*) (ofst (offset imm))
         (imm12 (delay :imm12 (ofst) (logand ofst #x00000fff)))
         (imm20 (delay :imm20 (ofst) (logand ofst #xfffff000))))
    (emit-vait
     (delay :lauipcj (ofst imm12 imm20)
       (if (cl:not (immp ofst 32))
           (rv-error "lfj: Upper Immediate value out of range." addr)
           (build-expr-code '(20 5 7)
                            (bits (if (= (logand imm12 #x800) #x800)
                                      ;; test for addi overflow/sign extension??
                                      (+ imm20 #x1000) imm20 )
                                  ;;simulate overflow/sign extension
                                  31 12) (regno rd) #x17))))
    (emit-vait
     (delay :lfj (imm12)
       (build-expr-code '(12 5 3 5 7) imm12 (regno rd) 1 (regno rd) #x3)))
    ))
;))

(defun lfv (rd imm)
  ;; (let ((ofst (offset imm)))
  ;;   (if (cl:and (ingtegerp imm) (immp ofst 12))
  ;;       (i.lv rd rd ofst)
  (let* ((addr *pc*) (ofst (offset imm))
         (imm12 (delay :imm12 (ofst) (logand ofst #x00000fff)))
         (imm20 (delay :imm20 (ofst) (logand ofst #xfffff000))))
    (emit-vait
     (delay :lauipcv (ofst imm12 imm20)
       (if (cl:not (immp ofst 32))
           (rv-error "lfv: Upper Immediate value out of range." addr)
           (build-expr-code '(20 5 7)
                            (bits (if (= (logand imm12 #x800) #x800)
                                      ;; test for addi overflow/sign extension??
                                      (+ imm20 #x1000) imm20 )
                                  ;;simulate overflow/sign extension
                                  31 12) (regno rd) #x17))))
    ;; TODO: add c.lv here
    (emit-vait
     (delay :lfv (imm12)
       (build-expr-code '(12 5 3 5 7) imm12 (regno rd) 2 (regno rd) #x3)))
    ))
;))

(defun sfb (rs rb imm)
  ;; (let ((ofst (offset imm)))
  ;;   (if (cl:and (ingtegerp imm) (immp ofst 12))
  ;;     (i.sb rs rb ofst)
  (let* ((addr *pc*) (ofst (offset imm))
         (imm12 (delay :imm12 (ofst) (logand ofst #x00000fff)))
         (imm20 (delay :imm20 (ofst) (logand ofst #xfffff000))))
    (emit-vait
     (delay :sauipcb (ofst imm12 imm20)
       (if (cl:not (immp ofst 32))
           (rv-error "sfb: Upper Immediate value out of range." addr)
           (build-expr-code '(20 5 7)
                            (bits (if (= (logand imm12 #x800) #x800)
                                      ;; test for addi overflow/sign extension??
                                      (+ imm20 #x1000) imm20 )
                                  ;;simulate overflow/sign extension
                                  31 12) (regno rb) #x17))))
    (emit-vait
     (delay :sfb (imm12)
       (build-expr-code '(7 5 5 3 5 7) (bits imm12 11 5) (regno rs) (regno rb)
                        0 (bits imm12 4 0) #x23)))
    ))
;))

(defun sfj (rs rb imm)
  ;; (let ((ofst (offset imm)))
  ;; (if (cl:and (ingtegerp imm) (immp ofst 12))
  ;;     (i.sj rs rb ofst)
  (let* ((addr *pc*) (ofst (offset imm))
         (imm12 (delay :imm12 (ofst) (logand ofst #x00000fff)))
         (imm20 (delay :imm20 (ofst) (logand ofst #xfffff000))))
    (emit-vait
     (delay :sauipcj (ofst imm12 imm20)
       (if (cl:not (immp ofst 32))
           (rv-error "sfj: Upper Immediate value out of range." addr)
           (build-expr-code '(20 5 7)
                            (bits (if (= (logand imm12 #x800) #x800)
                                      ;; test for addi overflow/sign extension??
                                      (+ imm20 #x1000) imm20 )
                                  ;;simulate overflow/sign extension
                                  31 12) (regno rb) #x17))))
    (emit-vait
     (delay :sfj (imm12)
       (build-expr-code '(7 5 5 3 5 7) (bits imm12 11 5) (regno rs) (regno rb)
                        1 (bits imm12 4 0) #x23)))
    ))
;))

(defun sfv (rs rb imm)
  ;; (let ((ofst (offset imm)))
  ;;   (if (cl:and (ingtegerp imm) (immp ofst 12))
  ;;       (i.sv rs rb ofst)
  (let* ((addr *pc*) (ofst (offset imm))
         (imm12 (delay :imm12 (ofst) (logand ofst #x00000fff)))
         (imm20 (delay :imm20 (ofst) (logand ofst #xfffff000))))
    (emit-vait
     (delay :sauipcv (ofst imm12 imm20)
       (if (cl:not (immp ofst 32))
           (rv-error "sfv: Upper Immediate value out of range." addr)
           (build-expr-code '(20 5 7)
                            (bits (if (= (logand imm12 #x800) #x800)
                                      ;; test for addi overflow/sign extension??
                                      (+ imm20 #x1000) imm20 )
                                  ;;simulate overflow/sign extension
                                  31 12) (regno rb) #x17))))
    (emit-vait
     (delay :sfv (imm12)
       (build-expr-code '(7 5 5 3 5 7) (bits imm12 11 5) (regno rs) (regno rb)
                        2 (bits imm12 4 0) #x23)))
    ))
;;))

;; TODO: is it possible to add comprsessed instructions? needs test c.jal offset 12?
(defun call (imm &optional (reg 'x1))
 "(call imm [reg x1])
  Call far away subroutine (pseudoinstruction for auipc reg and jalr reg)"
  (let* ((addr *pc*)
         (ofst (offset imm))
    ;;      )
    ;; (cond  ;; can use single base instruction if imm < 20-bits
    ;;   ((cl:and (numberp imm) (immp ofst 20) (zerop (logand ofst #x1)))
    ;;    (i.jal reg imm))
    ;;   (t
    ;;    (let (
         (imm12 (delay :imm12 (ofst) (logand ofst #x00000fff)))
         (imm20 (delay :imm20 (ofst) (logand ofst #xfffff000))))
    ;; (i.auipc reg
    ;;        ;; imm20 )
    ;;        (delay :call (imm12 imm20) (if (= (logand imm12 #x800) #x800)
    ;;                                     ;; test for addi overflow/sign extension??
    ;;                                       (+ imm20 #x1000) imm20 )))
    ;;                                     ;;simulate overflow/sign extension
    (emit-vait
     (delay :auicall (ofst imm12 imm20)
       (if (cl:not (immp ofst 32))
           (rv-error "call: Immediate value out of range" addr)
           (build-expr-code '(20 5 7) (bits (if (= (logand imm12 #x800) #x800)
                                            ;; test for addi overflow/sign extension??
                                                (+ imm20 #x1000) imm20 ) 31 12)
                                             ;;simulate overflow/sign extension
                                                (regno reg) #x17))))
    ;; (i.jalr reg reg imm12))
    (emit-vait
      (delay :jalrcall (imm12)
        (build-expr-code '(12 5 3 5 7) imm12 (regno reg) 0 (regno reg) #x67)))
    ;;)))
  ))

;; TODO: is it possible to add comprsessed instructions? needs test
(defun tail (imm &optional (treg 'x6))
 "(tail imm [treg 'x6])
  Tail call far away subroutine
   (pseudoinstruction for  (auipc x6) and (jalr x0 x6))"
  (let* ((addr *pc*)
         (ofst (offset imm))
    ;;      )
    ;; (cond ;; can use single base instruction if imm < 20-bit
    ;;   ((cl:and (numberp imm) (immp ofst 20) (zerop (logand ofst #x1)))
    ;;    (i.jal 'x0 imm))
    ;;   (t
    ;;    (let (
             (imm12 (delay :imm12 (ofst) (logand ofst #x00000fff)))
             (imm20 (delay :imm20 (ofst) (logand ofst #xfffff000))))
    ;; (i.auipc treg
    ;;        ;; imm20 )
    ;;        (delay :tail (imm12 imm20) (if (= (logand imm12 #x800) #x800)
    ;;                                     ;; test for addi overflow/sign extension??
    ;;                                     (+ imm20 #x1000) imm20 )))
    ;;                                     ;;simulate overflow/sign extension
         (emit-vait
          (delay :auitail (ofst imm12 imm20)
            (if (cl:not (immp ofst 32))
                (rv-error "tail: Immediate value out of range" addr)
                (build-expr-code '(20 5 7)
                                 (bits (if (= (logand imm12 #x800) #x800)
                                           ;; test for addi overflow/sign extension??
                                           (+ imm20 #x1000) imm20 ) 31 12)
                                 ;;simulate overflow/sign extension
                                 (regno treg) #x17))))
    ;; (i.jalr 'x0 treg imm12))
         (emit-vait
          (delay :jalrtail (imm12)
            (build-expr-code '(12 5 3 5 7) imm12 (regno treg) 0 (regno 'x0) #x67)))
    ;; )))
  ))
