
                    ;;;;;;;; Risc-V Instruction formats ;;;;;;;;

(in-package "RVASM")

        ;;;; Compressed 'C' module instruction formats ;;;;

;; cr - Compressed Register
(defun creg (rd rs1 op4 op2)
  "RISC-V compressed 'C' modules instruction format for instructions that
   use two x0..x15 registers ."
  (build-expr-code '(4 5 5 2) op4 (regno rd) (regno rs1) op2)
  ;)
  )


;; ci  - Compressed Immediate
(defun cimm (imm6 rd op1 op2)
  "RISC-V compressed 'C' modules instruction format for instructions that
   use an x0..x15 register and a 6-bit immediate ."
  (let ((addr *pc*))
    (delay :cimm (imm6)
      (if (immp imm6 6)
        (build-expr-code '(3 1 5 5 2) op1 (bits imm6 5) (regno rd) (bits imm6 4 0)
                                     op2);)
      (rv-error "cimm: Immediate value out of range." addr))
   )    )   )

;; ciw - Compressed Wide Immediate
(defun ciwid (imm8 crd op1 op2)
  "RISC-V compressed 'C' modules instruction format for instructions that
   use a c  (x8..x15) register and a 8-bit immediate ."
  (let ((addr *pc*))
    (delay :ciwid (imm8)
      (if (immp imm8 8)
        (build-expr-code '(3 2 4 1 1 3 2) op1 (bits imm8 7 6) (bits imm8 5 2)
                                              (bits imm8 0 ) (bits imm8 1)
                                              (cregno crd) op2);)
        (rv-error "ciwid: Immediate value out of range." addr))
      )  ))

;; cis - Compressed Immediate Small
(defun cismal (imm6 crd op1 op2 op3) ;; not used
  (let ((addr *pc*))
    (delay :cismal (imm6)
      (if (immp imm6 6)
        (build-expr-code '(3 1 2 3 5 2) op1 (bits imm6 5) op2 (cregno crd)
                                            (bits imm6 4 0) op3);)
        (rv-error "cismal: Immediate value out of range." addr))
      ) ))


;; cl - Compressed Load
(defun cload (imm5 crd crs1 op1 op2) ;; not used
  (let ((addr *pc*))
    (delay :cload (imm5)
      (if (immp imm5 5)
        (build-expr-code '(3 3 3 2 3 2) op1 (bits imm5 2 0) (cregno crs1)
                                            (bits imm5 4 3) (cregno crd) op2);)
        (rv-error "cload: Immediate value out of range." addr))
      ) ))

;; cs - Compressed Store
(defun cstore (imm5 crs1 crs2 op1 op2) ;; not used
  (let ((addr *pc*))
    (delay :cstore (imm5)
      (if (immp imm5 5)
        (build-expr-code '(3 3 3 1 1 3 2)
                           op1 (bits imm5 3 1) (cregno crs1)
                               (bits imm5 0) (bits imm5 4) (cregno crd) op2);)
        (rv-error "cstore: immediate value out of range." addr))
      ) ))

;; css - Compressed Stack Relative Store
(defun cstst (imm6 rs2 op1 op2) ;; not used
  (let ((addr *pc*))
    (delay :cstst (imm6)
      (if (immp imm6 6)
        (build-expr-code '(3 4 2 5 2) op1 (bits imm6 3 0) (bits imm6 5 4)
                                          (regno rs2) op2);)
        (rv-error "cstst: Immediate value out of range." addr))
      ) ))

;; ca - Compressed Arithmetic
(defun carith (op3 op1 op2 crd op2b crs2)
  "RISC-V compressed 'C' modules instruction format for instructions that
   use 2 'c' (x8..x15) registers."
  ;; (if (and (cregp rd) (cregp rs2))
        (build-expr-code '(3 1 2 3 2 3 2) op3 op1 op2 (cregno crd) op2b
                                          (cregno crs2) 1);)
      ;; (rv-error "C won't fit."))
   )

;; cj - Compressed Jump
(defun cjump (imm funct3 op)
  "RISC-V compressed 'C' modules instruction format for jump instructions that
  take only immediate arguments"
  (let ((addr *pc*) (ofst (offset imm)))
    (delay :cjump (ofst) ;(imm)
     ;; (let ((ofst (offset imm addr)))
      (cond ((not (immp ofst 12))
             (rv-error "cjump: Immediate value out of range." addr))
            ((not (zerop (logand ofst #x1)))
             (rv-error "cjump: Immediate value should be a multiple of 2." addr))
            (t
            (build-expr-code '(3 1 1 2 1 1 1 3 1 2)
                             funct3 (bits ofst 11) (bits ofst 4) (bits ofst 9 8)
                                    (bits ofst 10) (bits ofst 6) (bits ofst 7)
                                    (bits ofst 3 1)(bits ofst 5) op);)
        ;; (rv-error "Immediate value out of range.")
        ))
      )
  )
);;)

;; cb - Compressed Branch
(defun cbranch (imm crs1 funct3 op)
  "RISC-V compressed 'C' modules instruction format for branch instructions that
  take a 'c' (x8..x15) register and an immediate as arguments"
  (let ((addr *pc*) (ofst (offset imm))); addr)))
    (delay :cbranch (ofst)
      (cond ((not (immp ofst 9))
             (rv-error "cbranch: Immediate value out of range." addr))
            ((not (zerop (logand ofst #x1)))
             (rv-error "cbranch: Immediate value should be a multiple of 2." addr))
            (t
      ;; (if (imm? imm8 8)
             (build-expr-code '(3 1 2 3 2 2 1 2)
                              funct3 (bits ofst 8) (bits ofst 4 3) (cregno crs1)
                              (bits ofst 7 6) (bits ofst 2 1) (bits ofst 5) op)
             ;; (rv-error "immediate value out of range." addr))
             )
            ) )))


;; (defun cimm6 (rd imm op1 op2)
;;   (emit '(3 1 5 5 2) op1 (bits imm 5) (regno rd) (bits imm 4 0) op2))

;; (defun cimm6* (rd imm op1 op2 op3)
;;   (emit '(3 1 2 3 5 2) op1 (bits imm 5) op2 (cregno rd) (bits imm 4 0) op3))


;; (defun cimmed (imm12 rs1 funct3 rd op)
;;   (emit* '(12 5 3 5 7) imm12 (regno rs1) funct3 (regno rd) op))

;; (defun cregister (op3 op1 op2 rd op2b rs2)
;;   (cond
;;    ((and (cregp rd) (cregp rs2))
;;     (emit '(3 1 2 3 2 3 2) op3 op1 op2 (cregno rd) op2b (cregno rs2) 1))
;;    (t (error* "C won't fit"))))

        ;;;; base 'I' modlule formats ;;;;

(defun register (funct7 rs2 rs1 funct3 rd op)
  "RISC-V base 'I' modules instruction format for instructions that
   use three registers."
    (build-expr-code '(7 5 5 3 5 7) funct7 (regno rs2) (regno rs1) funct3
                                    (regno rd) op));)


(defun immed (imm12 rs1 funct3 rd op)
  "RISC-V base 'I' modules instruction format for instructions that
   use two registers and a 12-bit immediate 'imm12'."
  (let ((addr *pc*))
  (delay :immed (imm12)
      (if (immp imm12 12)
        (build-expr-code '(12 5 3 5 7) ;; imm12
                                      (logand imm12 #xfff)
                                      (regno rs1) funct3 (regno rd) op)
       ;; )
        (rv-error "Immed: Immediate value out of range." addr))
      ) ))

;; (defun immed (imm12 rs1 funct3 rd op)
;;   (cond
;;    ((immp imm12 12)
;;     (emit* '(12 5 3 5 7) (logand imm12 #xfff) (regno rs1) funct3 (regno rd) op))
;;    (t
;;     (error* "Immediate value out of range."))))

(defun branch (imm rs2 rs1 funct3 op)
  "RISC-V base 'I' modules instruction format for branch instructions that
   compare rs1 and rs2 and jump to immedate (label) 'imm'."
  (let ((addr *pc*) (ofst (offset imm)))
    (delay :branch (ofst) ; (imm)
      ;; (let ((ofst (offset imm addr)))
        (cond ((not (immp ofst 13))
               (rv-error "Branch: Immediate value out of range." addr))
              ((not (zerop (logand ofst 1)))
               (rv-error "Branch: Immediate should be a multiple of 2." addr))
              (t
             ;; (if (immp imm12 12)
      ;; (let ((ofst (offset imm12)))
               (build-expr-code '(1 6 5 5 3 4 1 7)
                                (bits ofst 12) (bits ofst 10 5) (regno rs2)
                                (regno rs1) funct3 (bits ofst 4 1) (bits ofst 11) op)
               )
      ;)
      ;; (rv-error "Branch: Immediate value out of range."))
        ;; )
              ))) );)

;; (defun branch (imm12 rs2 rs1 funct3 funct7)
;;   (let ((off (offset imm12)))
;;     (emit* '(1 6 5 5 3 4 1 7)
;;            (bits off 12) (bits off 10 5) (regno rs2)
;;            (regno rs1) funct3 (bits off 4 1) (bits off 11) funct7)))

(defun jump (imm rd)
  "RISC-V base 'I' modules instruction format for jump and link instructions that
   store the return address in rd and jump to immediate (label) 'imm'."
  (let ((addr *pc*) (ofst (offset imm)))
    (delay :jump (ofst) ;(imm)
      ;; (let ((ofst (offset imm addr)))
        (cond ((not (immp ofst 21))
               (rv-error "Jump: Immediate value out of range." addr))
              ((not (zerop (logand ofst 1)))
               (rv-error "Jump: Immediate should be a multiple of 2." addr))
              ;; (if (cl:and (immp imm20 21) (zerop (logand imm21 1)))
              (t
               (build-expr-code '(1 10 1 8 5 7) (bits ofst 20) (bits ofst 10 1)
                                                (bits ofst 11) (bits ofst 19 12)
                                                (regno rd) #x6f)
                                        ;)
               ;; (rv-error "Jump immediate value out of range no good." addr)
               )
              ))
    )  );)


;; (defun jump (imm20 imm10-1 imm11 imm19-12 rd op)
;;   (emit* '(1 10 1 8 5 7) imm20 imm10-1 imm11 imm19-12 rd op))

(defun store (imm12 src base funct3)
  "RISC-V base 'I' modules instruction format for instructions that store the contents
   of the address created by adding the contents of the 'base' register
   to the 12-bit immediate 'imm12' into the 'src' register."
  (let ((addr *pc*))
    (delay :store (imm12)
      (if (immp imm12 12)
        (build-expr-code '(7 5 5 3 5 7) (bits imm12 11 5) (regno src) (regno base)
                         funct3 (bits imm12 4 0) #x23)
                                        ;)
        (rv-error "Store: Immediate value out of range." addr))
      ) ))


(defun upperimm (imm32 rd op)
  "RISC-V base 'I' modules format for function that load 20-bit upper immediate in base 'I' modules.
  This funciton expects a 32-bit immediate that is a multiple of #x1000(4096)."
  (let ((addr *pc*))
    (delay :upperimm (imm32)
      (cond ((not (immp imm32 32))
             (rv-error "Upper Immediate value out of range." addr))
            ((not (zerop (logand imm32 #xfff )))
             (rv-error "Upper Immediate should be a multiple of #xfff(4096)." addr))
  ;; (if (cl:and (immp imm32 32) (zerop (logand imm #xfff)))
            (t (build-expr-code '(20 5 7) (bits imm32 31 12) (regno rd) op)
      ;; (rv-error "Upper immediate value out of range."))
      ;; (rv-error "Upper immediate value no good."))
             )))))

        ;;;; multiply formats ;;;;

(defun muldiv (rs2 rs1 funct3 rd op)
  "RISC-V format for multiplication and division instructions in
   Integer multiplication and division 'M' modules"
    (build-expr-code '(7 5 5 3 5 7) 1 (regno rs2) (regno rs1) funct3
                                      (regno rd) op));)


        ;;;; Control and Status Register formats ;;;;

(defun csrreg (csr rs1 funct3 rd)
  "RISC-V Control and Status Register 'ZiCSR' module format for CSR instructions
  that use registers."
  (let ((addr *pc*))
    (delay :csr (csr)
      (if (uimmp csr 12)
          (build-expr-code '(12 5 3 5 7) csr (regno rs1) funct3 (regno rd) #x73)
          (rv-error "csr: Wrong control and status register address" addr))
      )))


(defun csrimm (csr uimm5 funct3 rd)
  "RISC-V Control and Status Register 'ZiCSR' module format for CSR instructions
  that use 5-bit immediates."
  (let ((addr *pc*))
    (delay :csri (uimm5 csr)
      (cond ((not (uimmp uimm5 5))
             (rv-error "csri: Immediate value out of range." addr))
            ((not (uimmp csr 12))
             (rv-error "csri: Wrong control and status register address" addr))
            (t (build-expr-code '(12 5 3 5 7) csr uimm5 funct3 (regno rd) #x73))
     ))))

;

;; (defmacro define-c-immediate-type (name (param-list) &body body )
;; `(progn
;;    (degeneric ,name ,(pram-list)
;;               (:method ((,car ,(pram-list) ,@(cdr ,@(pram-list))
;;                          ,@body )))
;;               (:method ((,car ,(pram-list) ,@(cdr ,@(pram-list))
;;                               (delay (symbol-name name) (pram-list) (name (param-list) )))
;;                         )
;;                 )
;;               )
;;    )
;;   )
;; (defmacro define-c-immediate-type (name param-list &body body )
;; ;; `(progn
;;    `(defgeneric ,name ,param-list
;;      ;; ,@body
;;      (:method ((,(car param-list) integer) ,@(cdr param-list))
;;                          ,body );))
;;      (:method ((,(car param-list) promise) ,@(cdr param-list))
;;           (delay ,(symbol-name name) ,param-list (,name ,param-list) )))
;;      ;; (:method ((,car ,(pram-list) ,@(cdr ,@(pram-list))
;;                               ;; (delay (symbol-name name) (pram-list) (name (param-list) )))
;;                         ;; )
;;                 ;; )
;;               ;; )
;;    )
;; (defmacro define-c-immediate-type (name param-list &body body )
;; ;; `(progn
;;    `(defgeneric ,name ,param-list
;;      ;; ,@body
;;      (:method ((,(car param-list) integer) ,@(cdr param-list))
;;                          ,@body );))
;;      (:method ((,(car param-list) promise) ,@(cdr param-list))
;;           (delay ,(symbol-name name) ,(list (car param-list)) (,name ,param-list) )))
;;      ;; (:method ((,car ,(pram-list) ,@(cdr ,@(pram-list))
;;                               ;; (delay (symbol-name name) (pram-list) (name (param-list) )))
;;                         ;; )
;;                 ;; )
;;               ;; )
;;    )

;; This builds a generic function with methods for integers and promises/labels
;; (defmacro define-immediate-type (name param-list &body body )
;;    `(defgeneric ,name ,param-list
;;      (:method ((,(car param-list) integer) ,@(cdr param-list))
;;                          ,@body )
;;      (:method ((,(car param-list) promise) ,@(cdr param-list))
;;           (delay ,(symbol-name name) ,(list (car param-list)) (,name ,@param-list) )))
;;    )
