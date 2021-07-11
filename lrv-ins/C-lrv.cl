
                    ;;;;;;;; Compressed 32-bit Risc-V instructions ;;;;;;;;

(defpackage "C-32-RV"
  (:nicknames :c32)
  (:use :cl :rvasm)
  (:export  #:c.nop
            #:c.addi #:c.li
            #:c.addi4spn #:c.addi16sp
            #:c.lui
            #:c.andi
            #:c.srli #:c.srai #:c.slli
            #:c.mv #:c.add #:c.sub #:c.xor #:c.or #:c.and
            #:c.j #:c.jal #:c.jr #:c.jalr #:c.beqz #:c.bnez
            #:c.lv #:c.sv #:c.lvsp #:c.svsp
            #:c.ebreak
            ;; #:teq ;; test function
            ))

(in-package "C-32-RV")

;addi x0 x0 0(?)
(defun c.nop ()
  "(c.nop)
   Takes one cycle and advances *pc* by 2"
  (emit-jait (build-expr-code '(16) 1 ))
  )

        ;;;; Integer Computational Instructions ;;;;

;; c.addi - add immediate (addi rd rd value)
;; value ≠ 0 // rd ≠ x0
(defun c.addi (rd imm6)
  "(c.addi rd imm6)
   Add 6-bit immediate (sign extended to register width) to the value in rd
   then write the result to rd.
   (addi rd rd imm6) rd ≠ 0, imm ≠ 0."
  (if (not (zerop (regno rd)))
      (emit-jait
       (delay :c.addi (imm6)
              (if (not (zerop imm6))
                  (cimm imm6 rd 0 1)
                  (rv-error "c.addi: Immediate can not be 0."))))
      (rv-error "c.addi: Can not use x0 as the destination register.")))

  ;;  (cond ((zerop (regno rd))
  ;;         (rv-error "c.addi: Can not use x0 as the destination register."))
  ;;        ((zerop imm6);(= imm10 0)
  ;;         (rv-error "c.addi: Immediate can not be 0."))
  ;;        (t (emit-jait (cimm imm6 rd 0 1)))
  ;;      )
  ;; )

;; c.addi4spn - add immediate to stack pointer (addi rd x2 value)
;; value ≠ 0 // rd = x8..x15
(defun c.addi4spn (crd imm10)
  "(c.addi4spn rd imm10)
   c.addi4spn: Load rd with the sum of the stack pointer(x2) and the 10-bit
   immediate, which should be a multiple of 4 that is zero extended to register
   width. (addi rd x2 imm) rd = x8..x15, imm ≠ 0."
  (let ((addr *pc*))
    (if (cregp crd)
        (emit-jait
         (delay :c.addi4spn (imm10)
           (cond ((zerop imm10) ;(= imm10 0)
                  (rv-error "c.addi4spn: Immediate can not be 0." addr))
                 ((not (uimmp imm10 10)) ;; Note: uimmp!!
                  (rv-error "c.addi4spn: Immediate out of range." addr))
                 ((not (zerop (logand imm10 #x3)))
                  (rv-error "c.addi4spn: Immediate should be a multiple of 4." addr))
                 (t
                  (build-expr-code '(3 2 4 1 1 3 2)
                                   0 (bits imm10 5 4) (bits imm10 9 6)
                                       (bits imm10 2) (bits imm10 3) (cregno crd)
                                   0)
                  ))))
        (rv-error "c.addi4spn: Invalid compressed register (use x8..x15)." addr))
    ))
  ;; (if ;(cl:and
  ;;      (cregp rd) ;(not (= 0 imm8))) ; TODO move to ciwid
  ;;     (ciwid rd imm8 0 0)
  ;;     (rv-error "Invalid c.addi4spn expression.")
  ;; ))


;; c.addi16sp - add immediate to stack pointer (addi x2 x2 value)
;; // imm ≠ 0
(defun c.addi16sp (imm10)
  "(c.addi16sp imm10)
   c.addi16sp: Load stack pointer(x2) with the sum of the current value of the
   stack pointer(x2) and the 10-bit immediate which should be a muliple of 16
   that is sign extended to the register width.
   (addi x2 x2 imm) imm ≠ 0."
  (let ((addr *pc*))
    (emit-jait
     (delay :c.addi16sp (imm10)
       (cond ((zerop imm10);(= imm10 0)
              (rv-error "c.addi16sp: Immediate can not be 0." addr))
             ((not (zerop (logand imm10 #xf)))
              (rv-error "c.addi16sp: Immediate should be a multiple of 16."))
             ((not (immp imm10 10))
              (rv-error "c.addi16sp: Immediate value out of range."))
             (t
              (build-expr-code '(3 1 5 1 1 2 1 2)
                               3 (bits imm10 9) 2 (bits imm10 4)
                                 (bits imm10 6) (bits imm10 8 7)
                                 (bits imm10 5) 1)))
  )))
  )


;; c.li - load immediate (addi rd x0 value)
;; rd ≠ x0
(defun c.li (rd imm6)
  "(c.li rd imm6)
   Commpressed Load Immediate: Load 6-bit immediate (sign extended to register
   width) into rd. (addi rd x0 imm) rd ≠ 0."
   (if (not (zerop (regno rd)))
       (emit-jait (cimm imm6 rd 2 1))
       (rv-error "c.li: Can not use x0 as the destination register."))
     )

;; c.lui - load upper immediate (lui rd value)
;; // rd ≠ x0 x2 // imm ≠ 0
(defun c.lui (rd imm18)
  "(c.lui rd imm18)
   Commpressed load upper immediate: Load rd with non-zero immediate (which should be a
   multiple of 4096 or #x1000), sign extending the highest bit (bit 18) into
   all the higher bits of rd.
   (lui rd imm18) rd ≠ x0 or x4, imm ≠ 0."
  (let ((addr *pc*))
    (if (not (or (zerop (regno rd)) (= (regno rd) 2)))
        (emit-jait
         (delay :c.lui (imm18)
                (cond
                  ;; ((or (zerop (regno rd)) (= (regno rd) 2))
                   ;; (rv-error "c.lui: Invalid register (x0 or x2 invalid)"))
                  ((not (zerop (logand imm18 #xfff)))
                   (rv-error
                    "c.lui: Immediate should be a multiple of 4096/#x1000." addr))
                  ((not (immp imm18 18))
                   (rv-error "c.lui: Immediate value out of range." addr))
                  ((zerop imm18)
                   (rv-error "c.lui: Immediate can not be 0." addr))
                  (t
                   (build-expr-code '(3 1 5 5 2) 3 (bits imm18 17) (regno rd)
                                    (bits imm18 16 12) 1)))
                ))
        (rv-error "c.lui: Invalid register (x0 and x2 invalid)" addr)
        )))

;; c.andi - logical AND (immediate) (andi rd rd value)
;; //   rd  = x8 ... x15     ?? imm6 ≠ 0 ??
(defun c.andi (crd imm6)
  "(c.andi rd imm6)
   AND value in rd and the immediate (sign extended to register size) and write
   result to rd. (andi rd rd imm6) rd = x8..x15."
  (let ((addr *pc*))
   (if (cregp crd)
       (emit-jait ;; (cismal imm6 rd 4 2 1)
        (delay :c.andi (imm6)
          (cond ((not (immp imm6 6))
                 (rv-error "c.andi: Immediate value out of range." addr))
                (t
                  (build-expr-code '(3 1 2 3 5 2)
                                             4 (bits imm6 5) 2 (cregno crd)
                                               (bits imm6 4 0) 1))

                 )
          ))
     (rv-error "c.andi: Invalid compressed register, (use x8..x15)." addr))
  ))

        ;;;; register shift instructions ;;;;

;; c.srli - shift right logical (immediate) (srli rd rd value)
;; // rd = x8 ... x15 value = 1 .. 31  check summary
(defun c.srli (crd imm5)
  "(c.srli rd imm5)
   Compressed Shift right logical immediate: Shift contents of register right
   by immediate value and store result in rd.
   (srli rd rd imm), rd = x8..x15, imm = 1 .. 31."
  (let ((addr *pc*))
    (cond ((not (cregp crd))
           (rv-error "c.srli: Invalid compressed register, (use x8..x15)." addr))
          ((not (<= 1 imm5 31))
           (rv-error "c.srli: Immediate should be between 1 and 31." addr))
          (t
           (emit-jait (build-expr-code '(4 2 3 5 2) 8 0 (cregno crd) imm5 1))
           )
          )
  ))

;; c.srai - shift right arithmetic (Immediate) (srai rd rd value)
;; // rd = x8 ... x15 value = 1 .. 31  check summarY
(defun c.srai (crd imm5)
  "(c.srai rd imm5)
   Compressed Shift right aritmetic immediate: Shift contents of register right
   by immediate value, keeping the sign bit and store result in rd.
   (srai rd rd imm), rd = x8..x15, imm = 1 .. 31."
  (let ((addr *pc*))
    (cond ((not (cregp crd))
           (rv-error "c.srai: Invalid compressed register, (use x8..x15)." addr))
          ((not (<= 1 imm5 31))
           (rv-error "c.srai: Immediate should be between 1 and 31." addr))
          (t
           (emit-jait (build-expr-code '(4 2 3 5 2) 8 1 (cregno crd) imm5 1))
           )
          )))


;; c.slli - Shift left logical (immediate) (slli rd rd value)
;; // rd ≠ x0 value = 1 .. 31
(defun c.slli (rd imm5)
  "(c.slli rd imm5)
   Compressed Shift left logical immediate: Shift contents of register left
   by immediate value and store result in rd.
   (slli rd rd imm), imm = 1 .. 31."
  (let ((addr *pc*))
    (cond ((zerop (regno rd))
           (rv-error "c.slli: Can not use x0 as the destination register." addr))
          ((not (<= 1 imm5 31))
           (rv-error "c.slli: Immediate should be between 1 and 31." addr))
          (t
           (emit-jait (build-expr-code '(4 5 5 2) 0 (regno rd) imm5 2)))
  )))


        ;;;; Register only Computational instructions ;;;;

;; c.mv - move register to register (add rd x0 r2)
;; // rd  r2 ≠ x0
(defun c.mv (rd rs2)
  "(c.mv rd rs2)
   Move contents of rs2 to rd,
   rd/rs2 ≠ 0."
  (if (cl:and (not (zerop (regno rd))) (not (zerop (regno rs2))))
      (emit-jait (creg rd rs2 8 2))
      (rv-error "c.mv: Register x0 can not be used."))
  )

;; c.add - add register to register (add rd rd r2)
;; // rd  r2 ≠ x0
(defun c.add (rd rs2)
  "(c.add rd rs2)
   Add contents of rs2 to rd and store result in rd.
   (add rd rd rs2), rd/rs2 ≠ 0."
  (if (cl:and (not (zerop (regno rd))) (not (zerop (regno rs2))))
      (emit-jait (creg rd rs2 9 2))
      (rv-error "c.add: Register x0 can not be used."))
  )

;; c.and - AND register to register (and rd rd rs2),
;; //     rd r2 = x8..x15
(defun c.and (crd crs2)
  "(c.and rd rs2)
   AND rd and reg2 and store result in rd.
   (and rd rd rs2) rd/reg2 = x8..x15."
  (if (cl:and (cregp crd) (cregp crs2))
      (emit-jait (carith 4 0 3 crd 3 crs2))
      (rv-error "c.and: Invalid compressed register, (use x8..x15)."))
  )

;; c.sub - subtract register 2 from destination register (sub rd rd rs2),
;; //     rd r2 = x8 ... x15
(defun c.sub (crd crs2)
  "(c.sub rd rs2)
   Subtract reg2 from rd and store result in rd (sub rd rd rs2).
   rd/reg2  = x8..x15."
  (if (cl:and (cregp crd) (cregp crs2))
      (emit-jait (carith 4 0 3 crd 0 crs2))
      (rv-error "c.sub: Invalid compressed register, (use x8..x15)."))
  )

;; c.xor - xor register with register (xor rd rd rs2),
;; //     rd r2 = x8 ... x15
(defun c.xor (crd crs2)
  "(c.xor rd rs2)
   XOR rd and reg2 and store result in rd.
   (xor rd rd rs2) rd/reg2  = x8..x15."
  (if (cl:and (cregp crd) (cregp crs2))
      (emit-jait (carith 4 0 3 crd 1 crs2))
      (rv-error "c.xor: Invalid compressed register, (use x8..x15)."))
  )

;; c.or - OR register to register (or rd rd rs2),
;; //     rd r2 = x8 ... x15
(defun c.or (crd crs2)
  "(c.or rd rs2)
   OR rd and reg2 and store result in rd (or rd rd rs2).
   rd/reg2  = x8..x15."
  (if (cl:and (cregp crd) (cregp crs2))
      (emit-jait (carith 4 0 3 crd 2 crs2))
      (rv-error "c.or: Invalid compressed register, (use x8..x15)."))
  )
        ;;;; Jumps and branches ;;;;


;; c.j - Jump (pc-relative)  (jal  x0 offset)
(defun c.j (imm)
  "(c.j imm)
   Compressed jump: Jump to address described by immediate.
   (jal x0 offset), range = ±2KiB."
;;   (let ((addr *pc*)
        ;; (ofst (offset imm)))
    (emit-jait (cjump imm 5 1))
    )
;;     )

;;  jal x1 offset
(defun c.jal (imm)
  "(c.jal imm)
   Compressed Jump and link: Jump to address described by immediate storing
   return address(pc+2) in x1. (jal x1 offset), range = ±2KiB."
;;   (let ((addr *pc*)
;;         (ofst (offset imm)))
    (emit-jait (cjump imm 1 1))
    ;; )
  )

;; c.jr - jump register (jal x0  r1 0)
;; // r1 ≠ x0
(defun c.jr (rs1)
  "(c.jr rs1)
   Compressed jump register: Jump to address contained in register.
   (jalr x0 r1 0), rs1 ≠ 0."
  (if (not (zerop (regno rs1)))
      (emit-jait (build-expr-code '(3 1 5 5 2) 4 0 (regno rs1) 0 2))
      (rv-error "c.jr: Can not use x0 as source register."))
  )

;; c.jalr - jump register and link/call (jalr x1  r1 0) ??
;; // r1 ≠ x0
;jalr x1 rs1 0
(defun c.jalr (rs1)
  "(c.jalr rs1)
   Compressed jump and link register(call): Jump to address contained in register
   and write address following the jump (pc+2) to x1 (link register).
   (jalr x1 rs1 0), rs1 ≠ 0."
  (if (not (zerop (regno rs1)))
      (emit-jait (build-expr-code '(3 1 5 5 2) 4 1 (regno rs1) 0 2))
      (rv-error "c.jalr: can not use x0 as source register."))
  )

;; c.beqz - branch if (reg ) equal to zero (beq r1 x0 offset)
(defun c.beqz (crs1 imm)
  "(c.beqz rs1 imm)
   Branch to target address described by immediate if rs1 is equal to zero.
   (beq rs1 x0 imm), range = ±256B."
  (if (cregp crs1)
      (emit-jait (cbranch imm crs1 6 1))
      (rv-error "c.beqz: Invalid compressed register, (use x8..x15)."))
  )

;; c.bnez  - branch if (reg ) not equal to zero (bne r1 x0 offset)
(defun c.bnez (crs1 imm)
  "(c.bnez rs1 imm)
   Branch to target address described by immediate if rs1 is not equal to zero.
   (bne rs1 x0 imm), range = ±256B."
  (if (cregp crs1)
      (emit-jait (cbranch imm crs1 7 1))
      (rv-error "c.bnez: Invalid compressed register, (use x8..x15)."))
  )

        ;;;; Loads and stores ;;;;

;; c.lv - load vyte (lv rd r1 offset) //    rd & r1 = x8 ... x15
(defun c.lv (crd crs1 imm7)
  "(c.lv rd rs1 imm7)
   Compressed load vait: Load rd with the 32bit value from memory location derived
   from adding rsi to the 7-bit immediate a multiple of 4 zero extended to the
   register width. (lv rd rs1 imm7) rd/r1 = x8..x15."
  (let ((addr *pc*))
    (if (cl:and (cregp crd) (cregp crs1))
        (emit-jait
   ;; (cimm imm8 rd 4 4)
         (delay :c.lv (imm7)
           (cond ((not (uimmp imm7 7)) ;; Note: uimmp!!
                  (rv-error "c.lv: Immediate out of range." addr))
                 ((not (zerop (logand imm7 #x3)))
                  (rv-error "c.lv: Immediate should be a multiple of 4." addr))
                 (t
                  (build-expr-code '(3 3 3 1 1 3 2) 2 (bits imm7 5 3) (cregno crs1)
                                                      (bits imm7 2) (bits imm7 6)
                                                      (cregno crd) 0)
                  )
                 )
           )
         )
        (rv-error "c.lv: Invalid compressed register (use x8..x15)." addr))
    ))


;; c.sv - store vyte (sw rs2 rs1 offset)
;;    r1 & r2 = x8 ... x15
(defun c.sv (crs crb imm7)
  "(c.sv rs rb imm7)
   Compressed store vait: Stores 32 bit value in rs to memory location derived
   from adding rb to 7-bit immediate, a multiple of 4 zero extended to regsiter
   width. (sv rs rb imm7) rd/r1 = x8..x15."
   (let ((addr *pc*))
    (if (cl:and (cregp crs) (cregp crb))
        (emit-jait
         (delay :c.sv (imm7)
           (cond ((not (uimmp imm7 7)) ;; Note: uimmp!!
                  (rv-error "c.sv: Immediate out of range." addr))
                 ((not (zerop (logand imm7 #x3)))
                  (rv-error "c.sv: Immediate should be a multiple of 4." addr))
                 (t
                  (build-expr-code '(3 3 3 1 1 3 2) 6 (bits imm7 5 3) (cregno crb)
                                                      (bits imm7 2) (bits imm7 6)
                                                      (cregno crs) 0)
                  )
                 )
           )
         )
        (rv-error "c.lv: Invalid compressed register (use x8..x15)." addr))
    )   )

;; c.lvsp - load vyte from stack frame                 (lv rd offset(x2))
;; // rd ≠ x0
(defun c.lvsp (rd imm8)
  "(c.lvsp rd imm8)
   Compressed load vyte from stack pointer offset. Load rd with value from the
   memory address derived from adding value in the stack pointer to the
   8-bit immediate, a multiple of 4 zero extended to the register width.
   (lv rd x2 imm8) rd ≠ 0."
    ;; (if (eql rd 'x0) (rv-error "hei") (print "huh")
  (let ((addr *pc*))
    (if (not (zerop (regno rd)))
  ;;   (print rd)
  ;;   ;; (if (equal rd 'x0) (rv-error "hei")
        (emit-jait
   ;; (cimm imm8 rd 4 4)
         (delay :c.lvsp (imm8)
                (cond
                  ;; ((zerop (regno rd))
              ;; (eq rd 'x0) ;;doesn't work, why? 'x0 interned to other(rvasm) package?
              ;; (equal rd 'X0) ;;doesn't work, why?
                   ;; (rv-error "c.lvsp: Can not use x0 as the destination register." addr))
                  ((not (uimmp imm8 8)) ;; Note: uimmp!!
                   (rv-error "c.lvsp: Immediate out of range." addr))
                  ((not (zerop (logand imm8 #x3)))
                   (rv-error "c.lvsp: Immediate should be a multiple of 4" addr))
                  (t
                   (build-expr-code '(3 1 5 3 2 2) 2 (bits imm8 5) (regno rd)
                                                     (bits imm8 4 2)
                                                     (bits imm8 7 6) 2)
           )
             )
       )
     )
    (rv-error "c.lvsp: Can not use x0 as the destination register." addr)
    ))
    );)
    
;; c.svsp - store vyte to stack frame (sv r2 offset(x2))
(defun c.svsp (rs2 imm8)
  "(c.svsp rs2 imm8)
   Compressed store vait to stack pointer offset: Store 32 bit value in rs2 to the
   memory address derived from adding the stack pointer (x2) to the 8-bit immediate,
   a multiple of 4 zero extended to the register width.
   (sv rs2 x2 imm)."
  (let ((addr *pc*))
    (emit-jait
   ;; (cimm imm8 rd 4 4)
     (delay :c.svsp (imm8)
       (cond ((not (uimmp imm8 8)) ;; Note: uimmp!!
              (rv-error "c.svsp Immediate out of range." addr))
             ((not (zerop (logand imm8 #x3)))
              (rv-error "c.svsp Immediate should be a multiple of 4" addr))
             (t
              (build-expr-code '(3 4 2 5 2) 6 (bits imm8 5 2) (bits imm8 7 6)
                                              (regno rs2) 2)
              )
             )
       )
      )
    )
  )

        ;;;; Floating point instructions ;;;;

        ;;;; single floating point ;;;;

  ;; (c.flv       #b0110000000000000  #x6000  2  cl (frd rs1 imm6)) ;f //flv frd offset(rs1)
  ;; (c.fsv       #b1110000000000000  #xe000  2  cs (fr2 rs2 imm7))  ;f //fsv frs2 offset(rs1)
  ;; (c.flvsp     #b0110000000000010  #x6002  2  ci (frd imm6)) ;f //flv frd offset(x2)
  ;; (c.fsvsp     #b1110000000000010  #xe002  2  css (fr2 imm6)) ;f //fsv fr2 offset(x2)
  ;;

  ;; (c.flv       #b0110000000000000  #x6000  2  cl (frd rs1 imm6)) ;f //flv frd offset(rs1)
;; (c.flz       #b0010000000000000  #x2000  2  cl (frd rs1 imm6)) ;d //flz frd offset(rs1)
  ;; (c.fsv       #b1110000000000000  #xe000  2  cs (fr2 rs2 imm7))  ;f //fsv frs2 offset(rs1)
  ;; (c.fsz       #b1010000000000000  #xa000  2  cl (fr2 rs2 imm7))  ;d //fsz frs2 offset(rs1)

  ;; (c.flvsp     #b0110000000000010  #x6002  2  ci (frd imm6)) ;f //flv frd offset(x2)
  ;; (c.flzsp     #b0010000000000010  #x2002  2  ci (frd imm6)) ;d //fld frd offset(x2)
  ;; (c.fsvsp     #b1110000000000010  #xe002  2  css (fr2 imm6)) ;f //fsv fr2 offset(x2)
  ;; (c.fszsp     #b1010000000000010  #xa002  2  css (fr2 imm6)) ;d //fsd fr2 offset(x2)

        ;;;; Miscelleneous instructions ;;;;

 ;ebreak
(defun c.ebreak ()
  (emit-jait (build-expr-code '(4 5 5 2) 9 0 0 2))
  )

;; (defun teq (prd imm)
;;   (if (equal prd imm) (print "w") (print "n")))

(defpackage "C-64-I-RV"
  (:use :cl :rvasm :c-32-rv)
  (:shadow  #:c.srli #:c.srai #:c.slli )
  (:export  #:c.nop
            #:c.addi #:c.li
            #:c.addi4spn #:c.addi16sp
            #:c.lui
            #:c.andi
            #:c.mv #:c.add #:c.sub #:c.xor #:c.or #:c.and
            #:c.j #:c.jal #:c.jr #:c.jalr #:c.beqz #:c.bnez
            #:c.lv #:c.sv #:c.lvsp #:c.svsp
            #:c.ebreak
            ;; #:teq ;; test function

            ;; new or changed in C-64
            #:c.srli #:c.srai #:c.slli
            #:c.lz #:c.sz #:c.addiv #:c.lzsp #:c.szsp
            ))

(in-package "C-64-I-RV")

(defun c.lz ( )
  ""

)

(defun c.sz ( )
  ""

)

(defun c.addiv ( )
  ""

)

(defun c.srli ( )
  ""

)

(defun c.srai ( )
  ""

)

(defun c.slli ( )
  ""

)

(defun c.lzsp ( )
  ""

)

(defun c.szsp ( )
  ""

)

        ;;;; double floating point ;;;;
;; (c.flz       #b0010000000000000  #x2000  2  cl (frd rs1 imm6)) ;d //flz frd offset(rs1)
  ;; (c.fsz       #b1010000000000000  #xa000  2  cl (fr2 rs2 imm7))  ;d //fsz frs2 offset(rs1)
  ;; (c.flzsp     #b0010000000000010  #x2002  2  ci (frd imm6)) ;d //fld frd offset(x2)
  ;; (c.fszsp     #b1010000000000010  #xa002  2  css (fr2 imm6)) ;d //fsd fr2 offset(x2)
