
                    ;;;;;;;; Base and compressed instructions  ;;;;;;;;

(defpackage "I-C-32-RV"
  (:nicknames :ic32)
  (:description "32-bit instructions that use both I and C risc-v modules" )
  (:use :cl :rvasm :i-32-rv :c-32-rv)
  (:shadow or and rem)
  ;; (:shadowing-import-from :cl not)
  (:export #:nop #:mv #:jr #:jc #:ret
           #:addi #:lui #:li #:liu #:auipc #:slti #:sltiu #:xori #:ori #:andi
           #:slli #:srli #:srai
           #:add #:sub #:sll #:slt #:sltu #:xor #:srl #:sra #:or #:and
           #:j #:jal #:jalr #:beq #:bne #:blt #:bge #:bltu #:bgeu
           #:lb #:lj #:lv #:lbu #:lju #:sb #:sj #:sv
           #:fence #:ecall #:ebreak
            ))


(in-package :ic32)


        ;;;; Compressed Only Instructions ;;;;

 ;; (addi x0 x0 0)
(defun nop ()
  "(nop)
   Advances one cycle and advances *pc* by 2."
  (c.nop))

(defun mv (rd rs2)
  "(mv rd rs2)
   Move contents of register rs2 to rd."
  (c.mv rd rs2))

(defun jr (rs1)
  "(jr rs1)
   Jump to address in register (jal x0 r1 0)."
  (c.jr rs1))

(defun jc (imm)
  "(jc imm)
   Jump to address described by immediate"
  (c.j imm))

(defun ret ()
  "(ret)
   Return (Jump to) return address in ra (x1)"
  (c.jr 'ra))

        ;;;; Integer Computational instructions ;;;;

(defun addi (rd rs1 imm12)
  "(addi rd rs1 imm12)
   Sign-extended 12-bit immediate to register length and add to reg1
   placing the result in rd."
  (cond
    ((cl:and (integerp imm12) (immp imm12 6) (not (zerop imm12))
             (eq rd rs1) (not (zerop (regno rd))) (not (= (regno rd) 2)))
     (c.addi rd imm12))
    ((cl:and (integerp imm12) (immp imm12 6)
             ;; (eq rd rs1)
             (= (regno rs1) 0) (not (zerop (regno rd))))
     (c.li rd imm12))
    ((cl:and (integerp imm12) (immp imm12 10) (zerop (logand imm12 #xf)) ;; #xc0f
             (eq rd rs1) (= (regno rd) 2) (not (zerop imm12)))
     (c.addi16sp rd imm12))
    ((cl:and (integerp imm12) (immp imm12 10) (zerop (logand imm12 #x3)) ;; #xc03
             (eq rd rs1) (cregp rd)  (not (zerop imm12)))
     (c.addi4spn rd imm12))
    (t (i.addi rd rs1 imm12)
      ))
) ;cccc


;; Load Upper Immediate
(defun lui (rd imm32)
  "(lui rd imm32)
   Load Upper Immediate: Load most significant bits of register with immediate
   which should be a multiple of 4096 or #x1000, filling the lower 12 bits with
   zeros"
  (if (cl:and (integerp imm32) (immp imm32 17) (zerop (logand imm32 #xfff)) ;;#xfffc0fff
              ;; (eq rd rs1)
              (not (zerop imm32)) (not (zerop (regno rd))) (not (= (regno rd) 2)))
      (c.lui rd imm32)
      (i.lui rd imm32)
 )) ;c

;; Add Upper Immediate to PC
(defun auipc (rd imm32)
  "(auipc rd imm32)
   Add Upper Immediate to PC: Add the immediate
   (which should be a multiple of 4096 or #x1000) the program counter
   and load the result into the register."
  (i.auipc rd imm32)
  )

;; load immediate (derived)
;; This loads an immediate into rd, the bit imm[11] is added to imm20 to conteract
;; the sign extension of imm12
(defun li (rd imm)
  "(li rd imm)
   Load Immediate: Load signed immediate into rd"
  (if (cl:and (integerp imm) (immp imm 6) (not (zerop imm)) ) ;; (cregp rd))
      (c.li rd imm)
  ;; (delay :li (imm)
      (let ((imm12 (delay :imm12 (imm) (logand imm #x00000fff)))
            (imm20 (delay :imm20 (imm) (logand imm #xfffff000))))
        (lui rd
             ;; imm20 )
             (delay :li (imm12 imm20) (if (= (logand imm12 #x800) #x800)
                                     ;; test for addi overflow/sign extension??
                                              (+ imm20 #x1000) imm20 )))
                                     ;;simulate overflow/sign extension
        ;; (addi rd rd imm12)) ;; does not work because of (immp imm12 12) test
        ;; need to "force" it to accept #x800 as #x-800
        ;; TODO: is it possible to add comprsessed instructions?
        (emit-vait
         (delay :addli (imm12)
           (build-expr-code '(12 5 3 5 7) imm12 (regno rd) 0 (regno rd) #x13)))
        )
))


(defun liu (rd imm)
  "(liu rd imm)
   Load Immediate Unsigned: Load unsigned immediate into rd"
  (if (cl:and (integerp imm) (immp imm 6) (not (zerop imm)) )
      (c.li rd imm)
      (let ((imm12 (delay :imm12 (imm) (logand imm #x00000fff)))
            (imm20 (delay :imm20 (imm) (logand imm #xfffff000))))
        (emit-vait
         (let ((addr *pc*))
           (delay :liu (imm20)
             (cond ((not (uimmp imm20 32))
                    (rv-error "Upper Immediate value out of range." addr))
                   (t
                    (build-expr-code '(20 5 7)
                                     (bits (if (= (logand imm12 #x800) #x800)
                                     ;; test for addi overflow/sign extension??
                                               (+ imm20 #x1000)
                                     ;;simulate overflow/sign extension
                                               imm20 )
                                           31 12) (regno rd) #x37)
                    )   ))))
        ;; TODO: is it possible to add comprsessed instructions?
        (emit-vait
         (delay :addli (imm12)
           (build-expr-code '(12 5 3 5 7) imm12 (regno rd) 0 (regno rd) #x13)))
        )))

(defun slti (rd rs1 imm12)
  "(slti rd rs1 imm12)
   Set if less than: If reg1 is less than 12-bit immediate (sign-extended to
   register length), Load rd with value 1."
  (i.slti rd  rs1 imm12)
   )

(defun sltiu (rd rs1 imm12)
  "(sltiu rd rs1 imm12)
   If reg1 is less than unsigned representation of 12-bit sign-extended immediate
   load rd with value 1."
  (i.sltiu rd rs1 imm12))
;;) ;TODO WATCH!!

(defun xori (rd rs1 imm12)
  "(xori rd rs1 imm12)
   XOR 12-bit immediate (sign-extended to register length) with reg1 and
   load rd with result."
  (i.xori rd rs1 imm12)
   )

(defun ori (rd rs1 imm12)
  "(ori rd rs1 imm12)
    OR 12-bit immediate (sign-extended to register length) with reg1 and
    load rd with result."
  (i.ori rd rs1 imm12)
   )

(defun andi (rd rs1 imm12)
  "(andi rd rs1 imm12)
   AND 12-bit immediate (sign-extended to register length) with reg1 and load rd
   with result."
  (if (cl:and (integerp imm12) (immp imm12 6) (eq rd rs1) (cregp rd))
                ;;(not (zerop imm12)) ??
      (c.andi rd imm12)
      (i.andi rd rs1 imm12)
  ) );c

(defun slli (rd rs1 imm5)
  "(slli rd rs1 imm5)
   Shift left logical immediate: Shift the contents of reg1 left by the immediate
   value (between 0 and 31) and load rd with result."
  (if (cl:and (integerp imm5) (not (zerop imm5)) (eq rd rs1) )
      (c.slli rd imm5)
      (i.slli rd rs1 imm5))
  ) ;c

(defun srli (rd rs1 imm5)
  "(srli rd rs1 imm5)
   Shift right logical immediate: Shift the contents of reg1 right by the
   immediate value (between 0 and 31) and load rd with result."
  (if (cl:and (integerp imm5) (not (zerop imm5)) (eq rd rs1) (cregp rd) )
      (c.srli rd imm5)
      (i.srli rd rs1 imm5))
  ) ;c

(defun srai (rd rs1 imm5)
  "(srai rd rs1 imm5)
   Shift right arithmetic immediate: Shift the contents of reg1 right by the
   immediate value (between 0 and 31) keeping the sign bit and load rd with result."
  (if (cl:and (integerp imm5) (not (zerop imm5)) (eq rd rs1) (cregp rd) )
      (c.srai rd imm5)
      (i.srai rd rs1 imm5))
  ) ;c

        ;;;; Register only Computational Instructions ;;;;

(defun add (rd rs1 rs2)
  "(add rd rs1 rs2)
   Load rd with the sum of the contents of reg1 and reg2."
  (cond ((cl::and (eq rd rs1) (not (zerop (regno rd))) (not (zerop (regno rs2))))
         (c.add rd rs2))
        ((cl::and (not (zerop (regno rd))) (zerop (regno rs1))
                  (not (zerop  (regno rs2))))
         (c.mv rd rs2))
        (t (i.add rd rs1 rs2)))
  ) ;cc


(defun sub (rd rs1 rs2)
  "(sub rd rs1 rs2)
   Load rd with the result of subracting the contents of reg2 from reg1."
  (if (cl:and (eq rd rs1) (cregp rd) (cregp rs2))
      (c.sub (rd rs2))
      (i.sub rd rs1 rs2))
  ) ;c

(defun sll (rd rs1 rs2)
  "(sll rd rs1 rs2)
   Shift left logical: Load rd with the result of shifting the contents of reg1
  left by the amount in reg2 which must contain a number between 0 and 31."
  (i.sll rd rs1 rs2)
  )

(defun slt (rd rs1 rs2)
  "(slt rd rs1 rs2)
   Set if less than: Set the value of rd to 1 if the value in reg1 is less that
  the value in reg2, using signed comparison."
  (i.slt rd rs1 rs2)
  )

(defun sltu (rd rs1 rs2)
  "(sltu rd rs1 rs2)
   Set if less than unsigned: Set the value of rd to 1 if the value in reg1 is
   less that the value in reg2, using unsigned comparison."
  (i.sltu rd rs1 rs2)
  )

(defun xor (rd rs1 rs2)
  "(xor rd rs1 rs2)
   Load rd with the result of the logical xor of reg1 and reg2."
  (if (cl:and (eq rd rs1) (cregp rd) (cregp rs2))
      (c.xor rd rs2)
      (i.xor rd rs1 rs2))
   ) ;c

(defun srl (rd rs1 rs2)
  "(srl rd rs1 rs2)
   Shift right logical: Load rd with the result of shifting the contents of reg1
   right by the amount in reg2 which must contain a number between 0 and 31."
  (i.srl rd rs1 rs2)
  )

(defun sra (rd rs1 rs2)
  "(sra rd rs1 rs2)
   Shift right arithmetic: Load rd with the result of shifting the contents of
   reg1 right by the amount in reg2 which must contain a number between 0 and 31.
   Keeping the sign bit."
  (i.sra rd rs1 rs2)
  )

(defun or (rd rs1 rs2)
  "(or rd rs1 rs2)
   Load rd with the result of the logical or of reg1 and reg2."
  (if (cl:and (eq rd rs1) (cregp rd) (cregp rs2))
      (c.or rd rs2)
      (i.or rd rs1 rs2))
  ) ;c

(defun and (rd rs1 rs2)
  "(and rd rs1 rs2)
   Load rd with the result of the logical and of reg1 and reg2."
  (if (cl:and (eq rd rs1) (cregp rd) (cregp rs2))
      (c.and rd rs2)
      (i.and rd rs1 rs2))
  ) ;c

        ;;;; Jumps and branches ;;;;

(defun j (imm)
  "(j (imm)
   Jump to address described by immediate"
  (let ((ofst (offset imm)))
    (if (cl:and (integerp imm) (immp ofst 21) (zerop (logand ofst 1)))
        (c.j imm)
        (i.jal x0 imm))
  ))

(defun jal (rd imm)
  "(jal (rd imm)
   Jump and link: The return address (the address following the JAL i.e pc+4)
   is stored in rd. The destination address is the sum of
   The sign extended immediate (a multiple of 2) and the current address
   of the jump instruction."
  (let ((ofst (offset imm)))
    (cond ((cl:and (integerp imm) (immp ofst 21) (zerop (logand ofst 1))
                   (zerop (regno rd)))
           (c.j imm))
          ((cl:and (integerp imm) (immp ofst 21) (zerop (logand ofst 1))
                   (= (regno rd) 1))
           (c.jal imm))
          (t (i.jal rd imm)))
  )) ;cc

(defun jalr (rd rs1 imm12)
  "(jalr (rd rs1 imm12)
   Jump and link register: The return address
   (the address following the JALR i.e. pc+4) is stored in rd.
   The destination address is the sum of the sign extended immediate and
   the address stored in reg1."
  (cond ((cl:and (integerp imm12) (zerop imm12)
                 (zerop (regno rd)) (not (zerop (regno rs1))))
         (c.jr rs1))
        ((cl:and (integerp imm12) (zerop imm12)
                 (= (regno rd) 1) (not (zerop (regno rs1))))
         (c.jalr rs1))
        (t (i.jalr rd rs1 imm12))
  )) ;cc

(defun beq (rs1 rs2 imm)
  "(beq rs1 rs2 imm)
   Branch if equal: If rs1 is equal to rs2, the immediate is sign-extended
   and added to the value of the Program Counter
   (The value of the address following the instruction)
   to form the destination address."
  (let ((ofst (offset imm)))
    (if (cl:and (integerp imm) (immp ofst 8) (zerop (logand ofst #x1))
                (zerop (regno rs2)) (cregp rs1))
        (c.beqz rs1 imm)
        (i.beq rs1 rs2 imm)))
   );) ;c

(defun bne (rs1 rs2 imm)
  "(bne rs1 rs2 imm)
   Branch if not equal: If rs1 is not equal to rs2, the immediate is sign-extended
   and added to the value of the Program Counter
   (The value of the address following the instruction)
   to form the destination address."
  (let ((ofst (offset imm)))
    (if (cl:and (integerp imm) (immp ofst 8) (zerop (logand ofst #x1))
                (zerop (regno rs2)) (cregp rs1))
        (c.bnez rs1 imm)
        (i.bne rs1 rs2 imm)))
   );) ;c


(defun beqz (rs1 imm)
  "(beqz rs1 imm)
   Branch to address described by immediate if rs1 is equal to 0"
  (let ((ofst (offset imm)))
    (if (cl:and (integerp imm) (immp ofst 8) (zerop (logand ofst #x1)) ;; #x201
                (cregp rs1))
        (c.beqz rs1 imm)
        (i.beq rs1 'x0 imm))))

(defun bnez (rs1 imm)
  "(bnez rs1 imm)
   Branch to address described by immediate if rs1 not equal to 0"
  (let ((ofst (offset imm)))
    (if (cl:and (integerp imm) (immp ofst 8) (zerop (logand ofst #x1)) ;; #x201
                (cregp rs1))
        (c.bnez rs1 imm)
        (i.bne rs1 'x0 imm))))

(defun blt (rs1 rs2 imm)
  "(blt rs1 rs2 imm)
   Branch if less than: If rs1 is less than rs2 (using signed comparison),
   the immediate is sign-extended and added to the value of the Program Counter
   (The value of the address following the instruction)
   to form the destination address."
  (i.blt rs1 rs2 imm)
   )

(defun bge (rs1 rs2 imm)
  "(bge rs1 rs2 imm)
   Branch if greater or equal: If rs1 is greater than or equal to rs2 (using
   signed comparison),the immediate is sign-extended and added to the value of
   the Program Counter (The value of the address following the instruction)
   to form the destination address."
  (i.bge rs1 rs2 imm)
   )

(defun bltu (rs1 rs2 imm)
  "(bltu rs1 rs2 imm)
   Branch if less than unsigned: If rs1 is less than rs2 (using unsigned
   comparison),the immediate is sign-extended and added to the value of
   the Program Counter (The value of the address following the instruction)
   to form the destination address."
  (i.bltu rs1 rs2 imm)
   )

(defun bgeu (rs1 rs2 imm)
  "(bgeu rs1 rs2 imm)
   Branch if greater or equal: If rs1 is greater than or equal to rs2 (using
   unsigned comparison), the immediate is sign-extended and added to the value of
   the Program Counter (The value of the address following the instruction)
   to form the destination address."
  (i.bgeu rs1 rs2 imm)
   )

        ;;;; Loads and Stores ;;;;


(defun lb (rd rs1 imm12)
  "(lb rd rs1 imm12)
   Load byte: The 12-bit immediate is added to the value of reg1 to form a
   memory address An 8-bit value (byte) is fetched from this address and loaded
   into rd The value is sign exteded to the full length of the register."
  (i.lb rd rs1 imm12)
   )

(defun lj (rd rs1 imm12)
  "(lj rd rs1 imm12)
   Load jait: The 12-bit immediate is added to the value of reg1 to form a memory
   address. A 16-bit value (jait) is fetched from this address and loaded into rd.
   The value is sign exteded to the full length of the register."
  (i.lj rd rs1 imm12)
   )

(defun lv (rd rs1 imm12)
  "(lv rd rs1 imm12)
   Load vait: The 12-bit immediate is added to the value of reg1 to form a memory
   address. A 32-bit value (vait) is fetched from this address and loaded into rd.
   The value is sign exteded to the full length of the register."
  (cond ((cl:and (integerp imm12) (zerop (logand imm12 #xff83)) ;(immp imm12 7)
                 (cregp rd) (cregp rs1)) ;; the above checks it is within bounds
         (c.lv rd rs1 imm12))
        ((cl:and (integerp imm12) (zerop (logand imm12 #xff03)) ;(immp imm12 8) (zerop (logand imm12 #x3))
                 ;; (cregp rd)
                 (not (zerop (regno rd))) (= (regno rs1) 2))
         (c.lvsp rd imm12))
        (t (i.lv rd rs1 imm12)))
   );) ;cc

(defun lbu (rd rs1 imm12)
  "(lbu rd rs1 imm12)
   Load unsigned byte: The 12-bit immediate is added to the value of reg1 to
   form a memory address An 8-bit value is fetched from this address and loaded
   into rd. The value is zero exteded to the full length of the register."
  (i.lbu rd rs1 imm12)
   )

(defun lju (rd rs1 imm12)
  "(lju rd rs1 imm12)
   Load unsigned jait: The 12-bit immediate is added to the value of reg1 to
   form a memory address A 16-bit value is fetched from this address and loaded
   into rd. The value is zero exteded to the full length of the register."
  (i.lju rd rs1 imm12)
   )

(defun sb (rs rb imm12)
  "(sb rs rb imm12)
   Store byte: The 12-bit immediate is added to the value of rb to form a memory
   address The least significant 8-bit value is copied from rs and stored at
   this address."
  (i.sb rs rb imm12)
   )

(defun sj (rs rb imm12)
  "(sj rs rb imm12)
   Store jait: The 12-bit immediate is added to the value of rb to form a memory
   address The least significant 16-bit value is copied from rs1 and stored at
   this address."
  (i.sj rs rb imm12)
   )

(defun sv (rs rb imm12)
  "(sv rs rb imm12)
   Store vait: The 12-bit immediate is added to the value of rs2 to form a memory
   address, the least significant 32-bit value is copied from rs1 and stored at
   this address."
  (cond ((cl:and (integerp imm12) (zerop (logand imm12 #xff83)) ;(immp imm12 7)
                 (cregp rs) (cregp rb))
         (c.sv rs1 rs2 imm12))
        ((cl:and (integerp imm12) (zerop (logand imm12 #xff03)) ; (immp imm12 8) (zerop (lognand imm12 #x3))
                 ;; (cregp rs)
                 (= (regno rb) 2))
         (c.svsp rs imm12))
        (t (i.sv rs rb imm12)))
      );) ;cc

        ;;;; Miscellaneous Instructions ;;;;

(defun fence (succ pred)
  "(fence succ pred)"
  (i.fence (succ pred)))

;; (defun fence.(rd rs1 succ prod fm))
;; defun i.fence.(rd rs1 succ prod fm))
(defun ecall ()
  "(ecall)"
  (i.ecall))

(defun ebreak ()
  "(ebreak)"
  (c.ebreak)) ;c
;; defun i.ebreak ()) ;c

;; (defun csrrw (rd rs1))
;; defun i.csrrw (rd rs1))
;; (defun csrrs (rd rs1))
;; defun i.csrrs (rd rs1))
;; (defun csrrc (rd rs1))
;; defun i.csrrc (rd rs1))
;; (defun csrrwi (rd imm5))
;; defun i.csrrwi (rd imm5))
;; (defun csrrsi (rd imm5))
;; defun i.csrrsi (rd imm5))
;; (defun csrrci (rd imm5))
;; defun i.csrrci (rd imm5))


