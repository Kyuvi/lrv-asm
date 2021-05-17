                    ;;;;;;;; Base 32-bit Risc-V instructions ;;;;;;;;

(defpackage "I-32-RV"
  (:nicknames :i32)
  (:use :cl :rvasm)
  (:export #:i.addi #:i.lui #:i.auipc #:i.slti #:i.sltiu
           #:i.xori #:i.ori #:i.andi #:i.slli #:i.srli #:i.srai
           #:i.add #:i.sub #:i.sll #:i.slt #:i.sltu
           #:i.xor #:i.srl #:i.sra #:i.or #:i.and
           #:i.jal #:i.jalr #:i.beq #:i.bne #:i.blt #:i.bge #:i.bltu #:i.bgeu
           #:i.lb #:i.lj #:i.lv #:i.lbu #:i.lju #:i.sb #:i.sj #:i.sv
           #:i.fence #:i.ecall #:i.ebreak
           ;; #:iteq ;; test function
           ))

(in-package "I-32-RV")

        ;;;; Interger Computational Instructions ;;;;

(defun i.addi (rd rs1 imm12)
  "(i.addi rd rs1 imm12)
   Sign-extended 12-bit immediate to register length and add to reg1
   placing the result in rd."
  (emit-vait (immed imm12 rs1 0 rd #x13))
  )


;; Load Upper Immediate
(defun i.lui (rd imm32)
  "(i.lui rd imm32)
   Load Upper Immediate: Load most significant bits of register with immediate
   which should be a multiple of 4096 or #x1000, filling the lower 12 bits with
   zeros"
  (emit-vait (upperimm imm32 rd #x37))

  ;; (if
  ;;  ;; (zerop (logand imm #xfff))
  ;;  (immp imm32 20)
  ;;  (emit-vait (build-expr-code '(20 5 7) (bits imm32 19 0) (regno rd) #x37))
  ;;  (rv-error "lui immediate out of range."))
  )

;; Add Upper Immediate to PC
(defun i.auipc (rd imm32)
  "(i.auipc rd imm32)
   Add Upper Immediate to PC: Add the immediate
   (which should be a multiple of 4096 or #x1000) to the program counter
   and load the result into the register."
  (emit-vait (upperimm imm32 rd #x17))

  ;; (if (immp imm20 20)
  ;;     (emit-vait (build-expr-code '(20 5 7) (bits imm20 19 0) (regno rd) #x37))
  ;;     (rv-error "auipc immediate out of range."))
  )

(defun i.slti (rd rs1 imm12)
  "(i.slti rd rs1 imm12)
   Set if less than: If reg1 is less than the 12-bit immediate (sign-extended to
   register length), Load rd with value 1."
  (emit-vait (immed imm12 rs1 2 rd #x13))
  )

(defun i.sltiu (rd rs1 imm12)
  "(i.sltiu rd rs1 imm12)
   If reg1 is less than the unsigned representation of 12-bit sign-extended immediate
   load rd with value 1."
  (emit-vait (immed imm12 rs1 3 rd #x13)))  ;; turns out 'immed' is ok
;;   (let ((addr *pc*))
;;     (emit-vait (delay :i.sltiu (imm12)
;;                  (if (immp imm12 12) ;(uimmp imm12 12)
;;                      (build-expr-code '(12 5 3 5 7)
;;                                       ;; imm12
;;                                       (logand imm12 #xfff)
;;                                       (regno rs1) 3 (regno rd) #x13)
;;                      (rv-error "i.sltiu: Immediate value out of range" addr))))))
;; ;;) ;TODO 0 - 8191 WATCH!! changed to uimmp = 4095!! changed to immp !!!!


(defun i.xori (rd rs1 imm12)
  "(i.xori rd rs1 imm12)
   XOR 12-bit immediate (sign-extended to register length) with reg1 and
   load rd with result."
  (emit-vait (immed imm12 rs1 4 rd #x13))
  )

(defun i.ori (rd rs1 imm12)
  "(i.ori rd rs1 imm12)
    OR 12-bit immediate (sign-extended to register length) with reg1 and
    load rd with result."
  (emit-vait (immed imm12 rs1 6 rd #x13))
  )

(defun i.andi (rd rs1 imm12)
  "(i.andi rd rs1 imm12)
   AND 12-bit immediate (sign-extended to register length) with reg1 and load rd
   with result."
  (emit-vait (immed imm12 rs1 7 rd #x13))
  )

(defun i.slli (rd rs1 imm5)
  "(i.slli rd rs1 imm5)
   Shift left logical immediate: Shift the contents of reg1 left by the immediate
   value (between 0 and 31) and load rd with result."
 (if (<= 0 imm5 31)
     (emit-vait (build-expr-code '(7 5 5 3 5 7) 0 (bits imm5 4 0) (regno rs1) 1
                                                  (regno rd) #x13))
     (rv-error "Error i.slli immediate not between 0 and 31."))
  ) ;c

(defun i.srli (rd rs1 imm5)
  "(i.srli rd rs1 imm5)
   Shift right logical immediate: Shift the contents of reg1 right by the
   immediate value (between 0 and 31) and load rd with result."
 (if (<= 0 imm5 31)
     (emit-vait (build-expr-code '(7 5 5 3 5 7) 0 (bits imm5 4 0) (regno rs1) 5
                                                  (regno rd) #x13))
     (rv-error "Error i.srli immediate not between 0 and 31."))
  ) ;c

(defun i.srai (rd rs1 imm5)
  "(i.srai rd rs1 imm5)
   Shift right arithmetic immediate: Shift the contents of reg1 right by the
   immediate value (between 0 and 31) keeping the sign bit and load rd with result."
 (if (<= 0 imm5 31)
     (emit-vait (build-expr-code '(7 5 5 3 5 7) #x20 (bits imm5 4 0)
                                                     (regno rs1) 5
                                                     (regno rd) #x13))
     (rv-error "Error i.srai immediate not between 0 and 31."))
  ) ;c

        ;;;; Register only Computational Instructions;;;;

(defun i.add (rd rs1 rs2)
  "(i.add rd rs1 rs2)
   Load rd with the sum of the contents of reg1 and reg2."
 (emit-vait (register 0 rs2 rs1 0 rd #x33))
  ) ;cc

(defun i.sub (rd rs1 rs2)
  "(i.sub rd rs1 rs2)
   Load rd with the result of subracting the contents of reg2 from reg1."
 (emit-vait (register #x20 rs2 rs1 0 rd #x33))
  ) ;c

(defun i.sll (rd rs1 rs2)
  "(i.sll rd rs1 rs2)
   Shift left logical: Load rd with the result of shifting the contents of reg1
  left by the amount in reg2 which must contain a number between 0 and 31."
 (emit-vait (register 0 rs2 rs1 1 rd #x33))
  )

(defun i.slt (rd rs1 rs2)
  "(i.slt rd rs1 rs2)
   Set if less than: Set the value of rd to 1 if the value in reg1 is less that
  the value in reg2, using signed comparison."
 (emit-vait (register 0 rs2 rs1 2 rd #x33))
  )

(defun i.sltu (rd rs1 rs2)
  "(i.sltu rd rs1 rs2)
   Set if less than unsigned: Set the value of rd to 1 if the value in reg1 is
   less that the value in reg2, using unsigned comparison."
 (emit-vait (register 0 rs2 rs1 3 rd #x33))
  )

(defun i.xor (rd rs1 rs2)
  "(i.xor rd rs1 rs2)
   Load rd with the result of the logical xor of reg1 and reg2."
  (emit-vait (register 0 rs2 rs1 4 rd #x33))
  ) ;c

(defun i.srl (rd rs1 rs2)
  "(i.srl rd rs1 rs2)
   Shift right logical: Load rd with the result of shifting the contents of reg1
   right by the amount in reg2 which must contain a number between 0 and 31."
 (emit-vait (register 0 rs2 rs1 5 rd #x33))
  )

(defun i.sra (rd rs1 rs2)
  "(i.sra rd rs1 rs2)
   Shift right arithmetic: Load rd with the result of shifting the contents of
   reg1 right by the amount in reg2 which must contain a number between 0 and 31.
   Keeping the sign bit."
 (emit-vait (register #x20 rs2 rs1 5 rd #x33) ; 5 was orignally 2)
            )
  )

(defun i.or (rd rs1 rs2)
  "(i.or rd rs1 rs2)
   Load rd with the result of the logical or of reg1 and reg2."
 (emit-vait (register 0 rs2 rs1 6 rd #x33))
  ) ;c

(defun i.and (rd rs1 rs2)
  "(i.and rd rs1 rs2)
   Load rd with the result of the logical and of reg1 and reg2."
 (emit-vait (register 0 rs2 rs1 7 rd #x33))
  ) ;c

        ;;;; Jumps and branches ;;;;

(defun i.jal (rd imm)
  "(i.jal rd imm)
   Jump and link: The return address (the address following the JAL i.e pc+4)
   is stored in rd. The destination address is the sum of
   The sign extended immediate (a multiple of 2) and the current address
   of the jump instruction."
 (emit-vait (jump imm rd))
  ) ;ccc

(defun i.jalr (rd rs1 imm12)
  "(i.jalr rd rs1 imm12)
   Jump and link register: The return address
   (the address following the JALR i.e. pc+4) is stored in rd.
   The destination address is the sum of the sign extended immediate and
   the address stored in reg1."
 (emit-vait (immed imm12 rs1 0 rd #x67))
  ) ;c

(defun i.beq (rs1 rs2 imm12)
  "(i.beq rs1 rs2 imm12)
   Branch if equal: If rs1 is equal to rs2, the 12-bit immediate is sign-extended
   added to the value of the Program Counter
   (The value of the address following the instruction)
   to form the destination address."
  (emit-vait (branch imm12 rs2 rs1 0 #x63))
  ) ;c

(defun i.bne (rs1 rs2 imm12)
  "(i.bne rs1 rs2 imm12)
   Branch if not equal: If rs1 is not equal to rs2, the immediate is sign-extended
   and added to the value of the Program Counter
   (The value of the address following the instruction)
   to form the destination address."
  (emit-vait (branch imm12 rs2 rs1 1 #x63))
  ) ;c

(defun i.blt (rs1 rs2 imm12)
  "(i.blt rs1 rs2 imm12)
   Branch if less than: If rs1 is less than rs2 (using signed comparison),
   the immediate is sign-extended and added to the value of the Program Counter
   (The value of the address following the instruction)
   to form the destination address."
  (emit-vait (branch imm12 rs2 rs1 4 #x63))
  )

(defun i.bge (rs1 rs2 imm12)
  "(i.bge rs1 rs2 imm12)
   Branch if greater or equal: If rs1 is greater than or equal to rs2 (using
   signed comparison),the immediate is sign-extended and added to the value of
   the Program Counter (The value of the address following the instruction)
   to form the destination address."
  (emit-vait (branch imm12 rs2 rs1 5 #x63))
  )

(defun i.bltu (rs1 rs2 imm12)
  "(i.bltu rs1 rs2 imm12)
   Branch if less than unsigned: If rs1 is less than rs2 (using unsigned
   comparison),the immediate is sign-extended and added to the value of
   the Program Counter (The value of the address following the instruction)
   to form the destination address."
  (emit-vait (branch imm12 rs2 rs1 6 #x63))
  )

(defun i.bgeu (rs1 rs2 imm12)
  "(i.bgeu rs1 rs2 imm12)
   Branch if greater or equal: If rs1 is greater than or equal to rs2 (using
   unsigned comparison), the immediate is sign-extended and added to the value of
   the Program Counter (The value of the address following the instruction)."
  (emit-vait (branch imm12 rs2 rs1 7 #x63))
  )

        ;;;; Loads and stores ;;;;


(defun i.lb (rd rs1 imm12)
  "(i.lb rd rs1 imm12)
   Load byte: The 12-bit immediate is added to the value of reg1 to form a
   memory address An 8-bit value (byte) is fetched from this address and loaded
   into rd The value is sign exteded to the full length of the register."
  (emit-vait (immed imm12 rs1 0 rd 3))
  )

(defun i.lj (rd rs1 imm12)
  "(i.lj rd rs1 imm12)
   Load jait: The 12-bit immediate is added to the value of reg1 to form a memory
   address. A 16-bit value (jait) is fetched from this address and loaded into rd.
   The value is sign exteded to the full length of the register."
  (emit-vait (immed imm12 rs1 1 rd 3))
  )

(defun i.lv (rd rs1 imm12)
  "(i.lv rd rs1 imm12)
   Load vait: The 12-bit immediate is added to the value of reg1 to form a memory
   address. A 32-bit value (vait) is fetched from this address and loaded into rd.
   The value is sign exteded to the full length of the register."
  (emit-vait (immed imm12 rs1 2 rd 3))
  ) ;cc

(defun i.lbu (rd rs1 imm12)
  "(i.lbu rd rs1 imm12)
   Load unsigned byte: The 12-bit immediate is added to the value of reg1 to
   form a memory address An 8-bit value is fetched from this address and loaded
   into rd. The value is zero exteded to the full length of the register."
  (emit-vait (immed imm12 rs1 4 rd 3))
  )

(defun i.lju (rd rs1 imm12)
  "(i.lju rd rs1 imm12)
   Load unsigned jait: The 12-bit immediate is added to the value of reg1 to
   form a memory address A 16-bit value is fetched from this address and loaded
   into rd. The value is zero exteded to the full length of the register."
  (emit-vait (immed imm12 rs1 5 rd 3))
  )

(defun i.sb (rs1 rs2 imm12)
  "(i.sb rs1 rs2 imm12)
   Store byte: The 12-bit immediate is added to the value of rs2 to form a memory
   address The least significant 8-bit value is copied from rs1 and stored at
   this address (Note: registers inverted from standard)."
  (emit-vait (store imm12 rs1 rs2 0))
  )

(defun i.sj (rs1 rs2 imm12)
  "(i.sj rs1 rs2 imm12)
   Store jait: The 12-bit immediate is added to the value of rs2 to form a memory
   address The least significant 16-bit value is copied from rs1 and stored at
   this address (Note: registers inverted from standard)."
  (emit-vait (store imm12 rs1 rs2 1))
  )

(defun i.sv (rs1 rs2 imm12)
  "(i.sv rs1 rs2 imm12)
   Store vait: The 12-bit immediate is added to the value of rs2 to form a memory
   address The least significant 32-bit value is copied from rs1 and stored at
   this address (Note: registers inverted from standard)."
  (emit-vait (store imm12 rs1 rs2 2))
  ) ;cc

        ;;;; Miscellaneous Instructions ;;;;


(defun i.fence (succ pred)
  "(i.fence succ pred)"
  (if (cl:and (immp pred 4) (immp succ 4))
      (emit-vait (build-expr-code '(4 4 4 5 3 5 7) 0 pred succ 0 0 0 #xF))
      (rv-error "Fence immediate values out of range.")
  ))

;; (defun i.fence.i ())

(defun i.ecall ()
  "(i.ecall)"
  (emit-vait (build-expr-code '(16 16) 0 #x73)))

(defun i.ebreak ()
  "(i.ebreak)"
  (emit-vait (build-expr-code '(16 16) #x10 #x73))) ;c

;; (defun i.csrrw (rd rs1))
;; (defun i.csrrs (rd rs1))
;; (defun i.csrrc (rd rs1))
;; (defun i.csrrwi (rd imm5))
;; (defun i.csrrsi (rd imm5))
;; (defun i.csrrci (rd imm5))

;; (defun iteq (prd imm)
;;   (if (eq prd 'x0) (print "w") (print "n")))

(defpackage "I-64-RV"
  (:use :cl :rvasm :i-32-rv)
  (:shadow #:i.slli #:i.srli #:i.srai)
  (:export #:i.addi #:i.lui #:i.auipc #:i.slti #:i.sltiu
           #:i.xori #:i.ori #:i.andi
           #:i.add #:i.sub #:i.sll #:i.slt #:i.sltu
           #:i.xor #:i.srl #:i.sra #:i.or #:i.and
           #:i.jal #:i.jalr #:i.beq #:i.bne #:i.blt #:i.bge #:i.bltu #:i.bgeu
           #:i.lb #:i.lj #:i.lv #:i.lbu #:i.lju #:i.sb #:i.sj #:i.sv
           #:i.fence #:i.ecall #:i.ebreak

           ;; added or changed  in I-64
           #:i.lvu #:i.ld #:i.sd
           #:i.addiv #:i.slliv #:i.srliv #:i.sraiv
           #:i.addv #:i.subv #:i.sllv #:i.srlv #:i.srav
           #:i.slli #:i.srli #:i.srai
           ))

(in-package "I-64-RV")


(defun i.lvu ( )
  ""

)

(defun i.lz ( )
  ""

)

(defun i.sz ( )
  ""

)

(defun i.addiv ( )
  ""

)

(defun i.slliv ( )
  ""

)

(defun i.srliv ( )
  ""

)

(defun i.sraiv ( )
  ""

)

(defun i.addv ( )
  ""

)

(defun i.subv ( )
  ""

)

(defun i.sllv ( )
  ""

)

(defun i.srlv ( )
  ""

)

(defun i.srav ( )
  ""

)


(defun i.slli ( )
  ""

)

(defun i.srli ( )
  ""

)

(defun i.srai ( )
  ""

)
