
                    ;;;;;;;; Base and compressed instructions  ;;;;;;;;

(defpackage "I-C-32-RV"
  (:documentation "32-bit instructions that use both I and C risc-v modules" )
  (:nicknames :ic32)
  (:use :cl :rvreg :rvasm :i-32-rv :c-32-rv)
  (:shadow or and rem)
  ;; (:shadowing-import-from :cl not)
  (:export #:nop #:mv #:jr #:jc #:ret
           #:addi #:inc #:dec
           #:lui #:li #:liu #:auipc #:slti #:sltiu #:xori #:ori #:andi
           #:slli #:srli #:srai
           #:add #:sub #:sll #:slt #:sltu #:xor #:srl #:sra #:or #:and
           #:j #:jal #:jalr #:beq #:bne #:blt #:bge #:bltu #:bgeu
           #:lb #:lj #:lv #:lbu #:lju #:sb #:sj #:sv
           #:fence #:ecall #:ebreak
            ))


(in-package :ic32)

;; TODO: Make portable
;; (use (if (= *base-register-size* 32)
;;          :i-32-rv :i-64-rv))

;; TODO: Move i.x only procedures to rvidrv

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
  Jump to address described by the sign extended immediate 'imm'
  which should be in the range -1024(#x-400) to 1023(#x3FF) from the current
  position (pc)"
  (c.j imm))

(defun ret (&optional (rs1 ra))
 "(ret &optional (rs1 ra)
  Return (Jump to) return address in reg
  if no register is given use ra(x1)"
  (c.jr rs1))

        ;;;; Integer Computational instructions ;;;;

(defun addi (crd crs1 cimm12)
 "(addi crd crs1 cimm12)
  Sign-extended 12-bit immediate to register length and add to reg1
  placing the result in crd. Uses a compressed instruction if...
  crd = crs1 and  0 ≠ cimm12 <= 6 bits :c.addi
  crd ≠ x0 = crs1 and 0 ≠ cimm32 <= 6 bits :c.li
  crd = crs1 = x2 and 0 ≠ imm <= 10 bit multiple of 16 :c.addi16sp
  crd = x2 and 0 ≠ imm <= 10 bit multiple of 4 :c.addi4spn "
  (cond
    ((cl:and (integerp cimm12) (immp cimm12 6) (cl:not (zerop cimm12))
             (eq crd crs1) (cl:not (zerop (regno crd))) (cl:not (= (regno crd) 2)))
     (c.addi crd cimm12))
    ((cl:and (integerp cimm12) (immp cimm12 6)
             ;; (eq crd crs1)
             (= (regno crs1) 0) (cl:not (zerop (regno crd))))
     (c.li crd cimm12))
    ((cl:and (integerp cimm12) (immp cimm12 10) (zerop (logand cimm12 #xf)) ;; #xc0f
             (eq crd crs1) (= (regno crd) 2) (cl:not (zerop cimm12)))
     (c.addi16sp cimm12))
    ((cl:and (integerp cimm12) (uimmp cimm12 10) (zerop (logand cimm12 #x3)) ;; #xc03
             ;; (eq crd crs1) (cregp crd)  (cl:not (zerop cimm12)))
              (cregp crd) (= (regno crs1) 2)  (cl:not (zerop cimm12)))
     (c.addi4spn crd cimm12))
    (t (i.addi crd crs1 cimm12)
      ))
) ;cccc

(defun inc (reg &optional (imm12 1))
 "(inc reg &optional (imm12 1))
  increment REG by one or imm12 if given"
  (addi reg reg imm12))

(defun dec (reg &optional (imm12 1))
 "(dec reg &optional (imm12 1))
  decrement REG by by one or imm12 if given"
  (addi reg reg (- imm12)))

;; Load Upper Immediate
(defun lui (crd cimm32)
 "(lui crd cimm32)
  Load Upper Immediate: Load most significant bits of register with sign extended
  immediate 'cimm32'which should be a multiple of 4096 or #x1000,
  filling the lower 12 bits with zeros. Uses a compressed instruction if
  0 ≠ cimm32 <= 18 bits long and crd ≠ x0 or x2 :c.lui"
  (if (cl:and (integerp cimm32) (immp cimm32 18) (zerop (logand cimm32 #xfff)) ;;#xfffc0fff
             ;; (eq crd rs1)
              (cl:not (zerop cimm32)) (cl:not (zerop (regno crd)))
              (cl:not (= (regno crd) 2)))
      (c.lui crd cimm32)
      (i.lui crd cimm32)
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
(defun li (rd cimm)
 "(li rd cimm)
  Load Immediate: Load signed immediate into rd.
  assumes use of #h or #y read macros to avoid twos complement errors.
  Uses a compressed instruction if...
  crd ≠ x0 and 0 ≠ cimm32 <= 6 bits :c.li.
  crd ≠ x0|x2 and  0 ≠ cimm32 <= 18 bits :c.lui [and optionally c.li or i.addi]

"
  (cond ((cl:and (integerp cimm) (immp cimm 6) (cl:not (zerop cimm)) ) ;; (cregp rd))
         (c.li rd cimm))
        ((cl:and (integerp cimm) (immp cimm 12))
         (i.addi rd 'x0 cimm))
        ((cl:and (integerp cimm) (immp cimm 18) (zerop (logand cimm #xfff))
                 (cl:not (zerop (regno rd))) (cl:not (= (regno rd) 2)))
         (c.lui rd cimm))
        ((cl:and (integerp cimm) (immp cimm 18) (cl:not (zerop cimm))
                 (cl:not (zerop (regno rd))) (cl:not (= (regno rd) 2)))
         (let* ((imm12 (logand cimm #x00000fff))
                (imm18 (logand cimm #xfffff000))
                (imm18c (if (= (logand imm12 #x800) #x800)
                            (+ imm18 #x1000) imm18)))
           (emit-jait ;; c.lui
            (build-expr-code '(3 1 5 5 2) 3 (bits imm18c 17) (regno rd)
                             (bits imm18c 16 12) 1))
           (if  (immp imm12 6) ;; use  c.addi if possible TODO: TEST!! (NOT c.li)
               (emit-jait
                (build-expr-code '(3 1 5 5 2) 0 (bits imm12 5) (regno rd)
                                 (bits imm12 4 0) 1))
               (emit-vait ;; i.addi
                (build-expr-code '(12 5 3 5 7) imm12 (regno rd) 0 (regno rd) #x13)))
           ))
        ((cl:and (integerp cimm) (immp cimm 32) (zerop (logand cimm #xfff)))
         (i.lui rd cimm))
        ((cl:and (integerp cimm) (immp cimm 32))
         (let ((imm12 (logand cimm #x00000fff))
               (imm20 (logand cimm #xfffff000)))
           (emit-vait
            (build-expr-code '(20 5 7)
                             (bits (if (= (logand imm12 #x800) #x800)
                                       ;; test for addi overflow/sign extension??
                                       (+ imm20 #x1000) imm20 )
                                   ;;simulate overflow/sign extension
                                   31 12) (regno rd) #x37))
           (if  (immp imm12 6) ;; use  c.addi if possible TODO: TEST!! (NOT c.li)
                (emit-jait
                 (build-expr-code '(3 1 5 5 2) 0 (bits imm12 5) (regno rd)
                                  (bits imm12 4 0) 1))
           ;; (addi rd rd imm12)) ;; does not work because of (immp imm12 12) test
           ;; need to "force" it to accept #x800 as #x-800
                (emit-vait
                   (build-expr-code '(12 5 3 5 7) imm12 (regno rd) 0 (regno rd) #x13)))
        ))
  ;; (delay :li (cimm)
        (t (let ((addr *pc*)
                 (imm12 (delay :imm12 (cimm) (logand cimm #x00000fff)))
                 (imm20 (delay :imm20 (cimm) (logand cimm #xfffff000))))
        ;; (lui rd
        ;;      ;; imm20 )
        ;;      (delay :li (imm12 imm20) (if (= (logand imm12 #x800) #x800)
        ;;                              ;; test for addi overflow/sign extension??
        ;;                                       (+ imm20 #x1000) imm20 )))
        ;;                              ;;simulate overflow/sign extension
             (emit-vait
              (delay :li (cimm imm12 imm20)
                (if (cl:not (immp cimm 32))
                    (rv-error "li: Upper Immediate value out of range." addr)
                    (build-expr-code '(20 5 7)
                                     (bits (if (= (logand imm12 #x800) #x800)
                                               ;; test for addi overflow/sign extension??
                                               (+ imm20 #x1000) imm20 )
                                           ;;simulate overflow/sign extension
                                           31 12) (regno rd) #x37))))
             ;; (addi rd rd imm12)) ;; does not work because of (immp imm12 12) test
             ;; need to "force" it to accept #x800 as #x-800
             ;; TODO: is it possible to add comprsessed instructions?
             (emit-vait
              (delay :addli (imm12)
                (build-expr-code '(12 5 3 5 7) imm12 (regno rd) 0 (regno rd) #x13))))
        )
        ))
;; )


(defun liu (rd cimm)
 "(liu rd cimm)
  Load Immediate Unsigned: Load unsigned immediate into rd
  (NOTE: Depreciated in favour of using #h and #y read macros with li)
  Uses a compressed instruction if...
  crs1 = x0 and 0 ≠ cimm32 <= 6 bits :c.li"
   (if (cl:and (integerp cimm) (immp cimm 6) (cl:not (zerop cimm)) ) ;; (cregp rd))
       (c.li rd cimm)
      (let ((addr *pc*)
            (imm12 (delay :imm12 (cimm) (logand cimm #x00000fff)))
            (imm20 (delay :imm20 (cimm) (logand cimm #xfffff000))))
        (emit-vait
           (delay :liu (cimm imm12 imm20)
              (if (cl:not (uimmp cimm 32))
                  (rv-error "liu: Upper Immediate value out of range." addr)
                  (build-expr-code '(20 5 7)
                                   (bits (if (= (logand imm12 #x800) #x800)
                                     ;; test for addi overflow/sign extension??
                                             (+ imm20 #x1000) imm20 )
                                             ;;simulate overflow/sign extension
                                           31 12) (regno rd) #x37))))
        ;; TODO: is it possible to add comprsessed instructions?
        (emit-vait
         (delay :addliu (imm12)
           (build-expr-code '(12 5 3 5 7) imm12 (regno rd) 0 (regno rd) #x13))))))
;;)

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

(defun andi (crd crs1 cimm12)
 "(andi crd crs1 cimm12)
  AND 12-bit immediate (sign-extended to register length) with reg1 and load crd
  with result.
  Uses a compressed instruction if...
  crd = crs1 and 0 ≠ imm <= 6 bit :c.andi ."
  (if (cl:and (integerp cimm12) (immp cimm12 6) (eq crd crs1) (cregp crd))
                ;;(cl:not (zerop cimm12)) ??
      (c.andi crd cimm12)
      (i.andi crd crs1 cimm12)
  ) );c

(defun slli (crd crs1 cimm5)
 "(slli crd crs1 cimm5)
  Shift left logical immediate: Shift the contents of rs1 left by the immediate
  value (between 0 and 31) and load crd with result.
  Uses a compressed instruction if...
  crd = crs1 and 0 ≠ imm :c.slli ."
  (if (cl:and (integerp cimm5) (cl:not (zerop cimm5)) (eq crd crs1) )
      (c.slli crd cimm5)
      (i.slli crd crs1 cimm5))
  ) ;c

(defun srli (crd crs1 cimm5)
 "(srli crd crs1 cimm5)
  Shift right logical immediate: Shift the contents of reg1 right by the
  immediate value (between 0 and 31) and load crd with result.
  Uses a compressed instruction if...
  crd = crs1 and 0 ≠ imm :c.srli ."
  (if (cl:and (integerp cimm5) (cl:not (zerop cimm5)) (eq crd crs1) (cregp crd) )
      (c.srli crd cimm5)
      (i.srli crd crs1 cimm5))
  ) ;c

(defun srai (crd crs1 cimm5)
 "(srai crd crs1 cimm5)
  Shift right arithmetic immediate: Shift the contents of rs1 right by the
  immediate value (between 0 and 31) keeping the sign bit and load crd with result.
  Uses a compressed instruction if...
  crd = crs1 and 0 ≠ imm :c.srli ."
  (if (cl:and (integerp cimm5) (cl:not (zerop cimm5)) (eq crd crs1) (cregp crd) )
      (c.srai crd cimm5)
      (i.srai crd crs1 cimm5))
  ) ;c

        ;;;; Register only Computational Instructions ;;;;

(defun add (crd crs1 crs2)
 "(add crd crs1 crs2)
  Load crd with the sum of the contents of reg1 and reg2.
  Uses a compressed instruction if...
  crd = crs1 ≠ x0 ≠ crs2 :c.add
  crd ≠ x0 ≠ crs2, crs1 = x0 :c.mv ."
  (cond ((cl::and (eq crd crs1) (cl:not (zerop (regno crd)))
                  (cl:not (zerop (regno crs2))))
         (c.add crd crs2))
        ((cl::and (cl:not (zerop (regno crd))) (zerop (regno crs1))
                  (cl:not (zerop (regno crs2))))
         (c.mv crd crs2))
        (t (i.add crd crs1 crs2)))
  ) ;cc


(defun sub (crd crs1 crs2)
 "(sub crd crs1 crs2)
  Load crd with the result of subracting the contents of reg2 from reg1.
  Uses a compressed instruction if...
  crd = crs1 = x8..x15 = crs2 :c.sub "
  (if (cl:and (eq crd crs1) (cregp crd) (cregp crs2))
      (c.sub crd crs2)
      (i.sub crd crs1 crs2))
  ) ;sc

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

(defun xor (crd crs1 crs2)
 "(xor crd crs1 crs2)
  Load crd with the result of the logical xor of rs1 and rs2.
  Uses a compressed instruction if...
  crd = crs1 = x8..x15 = crs2 :c.xor "
  (if (cl:and (eq crd crs1) (cregp crd) (cregp crs2))
      (c.xor crd crs2)
      (i.xor crd crs1 crs2))
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

(defun or (crd crs1 crs2)
 "(or crd crs1 crs2)
  Load crd with the result of the logical or of reg1 and reg2.
  Uses a compressed instruction if...
  crd = crs1 = x8..x15 = crs2 :c.or. "
  (if (cl:and (eq crd crs1) (cregp crd) (cregp crs2))
      (c.or crd crs2)
      (i.or crd crs1 crs2))
  ) ;c

(defun and (crd crs1 crs2)
 "(and crd crs1 crs2)
  Load crd with the result of the logical and of reg1 and reg2.
  Uses a compressed instruction if...
  crd = crs1 = x8..x15 = crs2 :c.and. "
  (if (cl:and (eq crd crs1) (cregp crd) (cregp crs2))
      (c.and crd crs2)
      (i.and crd crs1 crs2))
  ) ;c

        ;;;; Jumps and branches ;;;;

(defun j (cimm)
 "(j cimm)
   Jump to address described by immediate
   Uses a compressed instruction if...
   cimm <= 12 bit multiple of 2 :c.j ."
  (let ((ofst (offset cimm)))
    (if (cl:and (integerp cimm) (immp ofst 12) (zerop (logand ofst 1)))
        (c.j cimm)
        (i.jal 'x0 cimm))
  ))

(defun jal (crd cimm)
 "(jal (crd cimm)
  Jump and link: The return address (the address following the JAL i.e pc+4)
  is stored in crd. The destination address is the sum of
  The sign extended immediate (a multiple of 2) and the current address
  of the jump instruction.
  Uses a compressed instruction if...
  crd = x0 and cimm <= 12 bit multiple of 2 :c.j
  crd = x1 and cimm <= 12 bit multiple of 2 :c.jal ."
  (let ((ofst (offset cimm)))
    (cond ((cl:and (integerp cimm) (immp ofst 12) (zerop (logand ofst 1))
                   (zerop (regno crd)))
           (c.j cimm))
          ((cl:and (integerp cimm) (immp ofst 12) (zerop (logand ofst 1))
                   (= (regno crd) 1))
           (c.jal cimm))
          (t (i.jal crd cimm)))
  )) ;cc

(defun jalr (crd crs1 cimm12)
 "(jalr (rd rs1 cimm12)
  Jump and link register: The return address
  (the address following the JALR i.e. pc+4) is stored in rd.
  The destination address is the sum of the sign extended immediate and
  the address stored in reg1.
  Uses a compressed instruction if...
  crd = x0 ≠ crs1 and cimm = 0 :c.jr
  crd = x1, crs1 ≠ x0 and cimm = 0 :c.jalr ."
  (cond ((cl:and (integerp cimm12) (zerop cimm12)
                 (zerop (regno crd)) (cl:not (zerop (regno crs1))))
         (c.jr rs1))
        ((cl:and (integerp cimm12) (zerop cimm12)
                 (= (regno crd) 1) (cl:not (zerop (regno crs1))))
         (c.jalr rs1))
        (t (i.jalr crd crs1 cimm12))
  )) ;cc

(defun beq (crs1 crs2 cimm)
 "(beq crs1 crs2 cimm)
  Branch if equal: If crs1 is equal to crs2, the immediate is sign-extended
  and added to the value of the Program Counter
  (The value of the address following the instruction)
  to form the destination address.
  Uses a compressed instruction if...
  crs1 = x8..x15, crs2 = x0 and cimm <= 8 bit multiple of 2 :c.beqz ."
  (let ((ofst (offset cimm)))
    (if (cl:and (integerp cimm) (immp ofst 8) (zerop (logand ofst #x1))
                (zerop (regno crs2)) (cregp crs1))
        (c.beqz crs1 cimm)
        (i.beq crs1 crs2 cimm)))
   );) ;c

(defun bne (crs1 crs2 cimm)
 "(bne crs1 crs2 cimm)
  Branch if not equal: If crs1 is not equal to crs2, the immediate is sign-extended
  and added to the value of the Program Counter
  (The value of the address following the instruction)
  to form the destination address.
  Uses a compressed instruction if...
  crs1 = x8..x15, crs2 = x0 and cimm <= 8 bit multiple of 2 :c.bnez ."
  (let ((ofst (offset cimm)))
    (if (cl:and (integerp cimm) (immp ofst 8) (zerop (logand ofst #x1))
                (zerop (regno crs2)) (cregp crs1))
        (c.bnez crs1 cimm)
        (i.bne crs1 crs2 cimm)))
   );) ;c


(defun beqz (crs1 cimm)
 "(beqz crs1 cimm)
  Branch to address described by immediate if crs1 is equal to 0
  Uses a compressed instruction if...
  crs1 = x8..x15, and cimm <= 8 bit multiple of 2 :c.beqz ."
  (let ((ofst (offset cimm)))
    (if (cl:and (integerp cimm) (immp ofst 8) (zerop (logand ofst #x1)) ;; #x201
                (cregp crs1))
        (c.beqz crs1 cimm)
        (i.beq crs1 'x0 cimm))))

(defun bnez (crs1 cimm)
 "(bnez crs1 cimm)
  Branch to address described by immediate if crs1 not equal to 0
  Uses a compressed instruction if...
  crs1 = x8..x15, and cimm <= 8 bit multiple of 2 :c.bnez ."
  (let ((ofst (offset cimm)))
    (if (cl:and (integerp cimm) (immp ofst 8) (zerop (logand ofst #x1)) ;; #x201
                (cregp crs1))
        (c.bnez crs1 cimm)
        (i.bne crs1 'x0 cimm))))

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
  memory address. An 8-bit value (byte) is fetched from this address and loaded
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

(defun lv (crd crs1 cimm12)
 "(lv crd crs1 cimm12)
  Load vait: The 12-bit immediate is added to the value of rs1 to form a memory
  address. A 32-bit value (vait) is fetched from this address and loaded into crd.
  The value is sign exteded to the full length of the register.
  Uses a compressed instruction if...
  crd = crs1 = x8..x15, and cimm <= 7 bit multiple of 4 :c.lv
  crd ≠ 0, crs1 = x2, and cimm <= 8 bit multiple of 4 :c.lvsp. "
  (cond ((cl:and (integerp cimm12) (zerop (logand cimm12 #xff83)) ;(immp cimm12 7)
                 (cregp crd) (cregp crs1)) ;; the above checks it is within bounds
         (c.lv crd crs1 cimm12))
        ((cl:and (integerp cimm12) (zerop (logand cimm12 #xff03)) ;(immp cimm12 8) (zerop (logand cimm12 #x3))
                 ;; (cregp crd)
                 (cl:not (zerop (regno crd))) (= (regno crs1) 2))
         (c.lvsp crd cimm12))
        (t (i.lv crd crs1 cimm12)))
   );) ;cc

(defun lfv (crd imm)
  (let ((ofst (offset imm)))
    (if (cl:and (ingtegerp imm) (immp ofst 32))
        (let* ((imm12 (logand ofst #x00000fff))
               (imm20 (logand ofst #xfffff000)))
          (emit-vait ;; auipc
           (build-expr-code '(20 5 7)
                            (bits (if (= (logand imm12 #x800) #x800)
                                      ;; test for addi overflow/sign extension??
                                      (+ imm20 #x1000) imm20 )
                                  ;;simulate overflow/sign extension
                                  31 12) (regno crd) #x17))
          (if (cl:and (uimmp imm12 7) (zerop (logand imm12 #x3)) (cregp crd))
              (emit-jait ;; c.lv
               (build-expr-code '(3 3 3 1 1 3 2) 2 (bits imm12 5 3) (cregno crd)
                                (bits imm12 2) (bits imm12 6)
                                (cregno crd) 0))
              (emit-vait ;; i.lv
               (build-expr-code '(12 5 3 5 7) imm12 (regno crd) 2 (regno crd)
                                               #x3)))
              )
        (let* ((addr *pc*)
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
                                        31 12) (regno crd) #x17))))
          (emit-vait
           (delay :lfv (imm12)
             (build-expr-code '(12 5 3 5 7) imm12 (regno crd) 2 (regno crd) #x3)))
          ))
    ))

;;

(defun lbu (rd rs1 imm12)
 "(lbu rd rs1 imm12)
  Load unsigned byte: The 12-bit immediate is added to the value of reg1 to
  form a memory address. An 8-bit value is fetched from this address and loaded
  into rd. The value is zero exteded to the full length of the register."
  (i.lbu rd rs1 imm12)
  )

(defun lju (rd rs1 imm12)
 "(lju rd rs1 imm12)
  Load unsigned jait: The 12-bit immediate is added to the value of reg1 to
  form a memory address. A 16-bit value is fetched from this address and loaded
  into rd. The value is zero exteded to the full length of the register."
  (i.lju rd rs1 imm12)
  )

(defun sb (rs rb imm12)
 "(sb rs rb imm12)
  Store byte: The 12-bit immediate is added to the value of rb to form a memory
  address. The least significant 8-bit value is copied from rs and stored at
  this address."
  (i.sb rs rb imm12)
  )

(defun sj (rs rb imm12)
 "(sj rs rb imm12)
  Store jait: The 12-bit immediate is added to the value of rb to form a memory
  address. The least significant 16-bit value is copied from rs1 and stored at
  this address."
  (i.sj rs rb imm12)
  )

(defun sv (crs crb cimm12)
 "(sv crs crb cimm12)
  Store vait: The 12-bit immediate is added to the value of ccrb to form a memory
  address. The least significant 32-bit value is copied from crs and stored at
  this address.
  Uses a compressed instruction if...
  crd = crs1 = x8..x15, and cimm <= 7 bit multiple of 4 :c.sv
  crd ≠ 0, crs1 = x2, and cimm <= 8 bit multiple of 4 :c.svsp. "
  (cond ((cl:and (integerp cimm12) (zerop (logand cimm12 #xff83)) ;(immp cimm12 7)
                 (cregp crs) (cregp crb))
         (c.sv crs crb cimm12))
        ((cl:and (integerp cimm12) (zerop (logand cimm12 #xff03)) ; (immp cimm12 8) (zerop (logand cimm12 #x3))
                 ;; (cregp crs)
                 (= (regno crb) 2))
         (c.svsp crs cimm12))
        (t (i.sv crs crb cimm12)))
      );) ;cc


(defun sfv (crs crb imm)
  (let ((ofst (offset imm)))
    (if (cl:and (ingtegerp imm) (immp ofst 32))
        (let* ((imm12 (logand ofst #x00000fff))
               (imm20 (logand ofst #xfffff000)))
          (emit-vait
           (build-expr-code '(20 5 7)
                            (bits (if (= (logand imm12 #x800) #x800)
                                      ;; test for addi overflow/sign extension??
                                      (+ imm20 #x1000) imm20 )
                                  ;;simulate overflow/sign extension
                                  31 12) (regno crb) #x17))
          (if (cl:and (uimmp imm12 7) (zerop (logand imm12 #x3)) (cregp crb))
              (emit-jait ;; c.sv
               (build-expr-code '(3 3 3 1 1 3 2) 6 (bits imm12 5 3) (cregno crb)
                                (bits imm12 2) (bits imm12 6)
                                (cregno crs) 0))

              (emit-vait ;; i.sv
               (build-expr-code '(7 5 5 3 5 7) (bits imm12 11 5) (regno crs)
                                (regno crb) 2 (bits imm12 4 0) #x23))
              ))
        (let* ((addr *pc*)
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
                                        31 12) (regno crb) #x17))))
          (emit-vait
           (delay :sfv (imm12)
             (build-expr-code '(7 5 5 3 5 7) (bits imm12 11 5) (regno crs)
                              (regno crb) 2 (bits imm12 4 0) #x23)))
          ))
    ))

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
