
            ;;;;;;;; Multiply and divide 32-bit Risc-V instructions ;;;;;;;;

(defpackage "M-32-RV"
  (:nicknames :m32)
  (:use :cl :rvasm)
  (:shadow rem)
  ;; (:export #:m.mul #:m.mulh #:m.mulhsu #:m.mulhu #:m.div #:m.divu #:m.rem #:m.remu
  (:export #:mul #:mulh #:mulhsu #:mulhu #:div #:divu #:rem #:remu
           ))

   (in-package "M-32-RV")


(defun mul (rd rs1 rs2)
 "(mul rd rs1 rs2)
  Multiply the contents of reg1 and reg2 and load rd with the result.
  Overflow is ignored, so rd is only loaded with the lower half of the result
  rd ≠ rs1 or rs2."
  (emit-vait (muldiv rs2 rs1 0 rd #x33))
)

(defun mulh (rd rs1 rs2)
 "(mulh rd rs1 rs2)
  Multiply the contents of reg1 and reg2 and load rd with the
  upper half (high bits) of the result of the signed multiplication
  rd ≠ rs1 or rs2."
  (emit-vait (muldiv rs2 rs1 1 rd #x33))
)

(defun mulhsu (rd rs1 rs2)
 "(mulhsu rd rs1 rs2)
  Multiply the contents of reg1 and reg2 and load rd with the
  upper half (high bits) of the result of the multiplication of
  a signed rs1 and and unsigned rs2.  rd ≠ rs1 or rs2."
  (emit-vait (muldiv rs2 rs1 2 rd #x33))
)

(defun mulhu (rd rs1 rs2)
 "(mulhu rd rs1 rs2)
  Multiply the contents of reg1 and reg2 and load rd with the
  upper half (high bits) of the result of the multiplication of
  a signed rs1 and and unsigned rs2.  rd ≠ rs1 or rs2."
  (emit-vait (muldiv rs2 rs1 3 rd #x33))
)

(defun div (rd rs1 rs2)
 "(div rd rs1 rs2)
  Divide the contents of reg 1 by the contents of reg2 and
  load rd with the quotient. Both values are signed
  Errors: -2(31)/-1=+2(31), x/0=-1(#xffffffff)."
  (emit-vait (muldiv rs2 rs1 4 rd #x33))
)

(defun divu (rd rs1 rs2)
 "(divu rd rs1 rs2)
  Divide the contents of reg 1 by the contents of reg2 and
  load rd with the quotient. Both values are unsigned
   x/0=+2(32)-1."
  (emit-vait (muldiv rs2 rs1 5 rd #x33))
)

(defun rem (rd rs1 rs2)
 "(rem rd rs1 rs2)
  Divide the contents of reg 1 by the contents of reg2 and
  load rd with the remainder. Both values are signed
  Errors: -2(31)/-1=0(correct result), x/0=a."
  (emit-vait (muldiv rs2 rs1 6 rd #x33))
)

(defun remu (rd rs1 rs2)
 "(remu rd rs1 rs2)
  Divide the contents of reg 1 by the contents of reg2 and
  load rd with the quotient. Both values are unsigned
  Errors:x/0=a."
  (emit-vait (muldiv rs2 rs1 7 rd #x33))
)

(defpackage "M-64-RV"
  (:use :cl :rvasm :m-32-rv)
  (:shadowing-import-from :m-32-rv rem)
  ;; (:export #:m.mul #:m.mulh #:m.mulhsu #:m.mulhu #:m.div #:m.divu #:m.rem #:m.remu
  (:export #:mul #:mulh #:mulhsu #:mulhu #:div #:divu #:rem #:remu

           ;; added or changed  in M-64
           ;; #:m.mulv #:m.divv #:m.divuv #:m.remv #:m.remuv
           #:mulv #:divv #:divuv #:remv #:remuv
           ))

   (in-package "M-64-RV")


(defun mulv ( )
 ""

)

(defun divv ( )
 ""

)

(defun divuv ( )
 ""

)

(defun remv ( )
 ""

)

(defun remuv ( )
 ""

)
