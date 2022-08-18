(defpackage "RV-REGISTERS"
  (:nicknames :rvreg)
  (:documentation
   "Binding of register names to instances of register classes with
    corresponding values")
  (:use :cl ) ;; :clrv)
  (:import-from :clrv def-multiple-constants )
  ;; (:shadowing-import-from :clrv def-multiple-constants )
  (:export
           #:rv-reg #:get-reg-val #:rv-c-reg #:get-c-reg-val
           #:rv-temp-reg #:rv-save-reg #:rv-arg-reg
           #:rv-base-reg #:rv-e-reg #:rv-cb-reg
           #:rv-base-arg-reg #:rv-base-save-reg #:rv-base-temp-reg
           #:rv-e-temp-reg #:rv-cb-arg-reg #:rv-cb-save-reg
  ))

(in-package "RV-REGISTERS")

        ;;;; Register class definition  ;;;;

(defclass rv-reg ()
  ((reg-value :type '(unsigned-byte 5) :reader get-reg-val :initarg :val)))

;; (defstruct rv-reg (:conc-name ))

(defclass rv-c-reg (rv-reg)
   ((c-reg-value :type '(unsigned-byte 3) :reader get-c-reg-val :initarg :cval))
   ;; ((c-reg-value :type '(unsigned-byte 3) :accessor get-c-reg-val :initarg :cval))
  )

(defmethod initialize-c-reg :after ((r rv-c-reg) &rest initargs)
  "Method to initialize c-reg-value slot from reg-value slot"
  ;; ;; Works on slots defined as readers or accessors
  ;; (setf (slot-value r 'c-reg-value)
  ;;       (logand (slot-value r 'reg-value) #x7)))

  ;; ;; Works on slots defined as readers or accessors
  (with-slots (reg-value c-reg-value) r
    (setf c-reg-value (logand reg-value #x7))))

  ;;   ;; Only works if written slot is defined as an accessor
  ;; (with-accessors ((val get-reg-val) (cval get-c-reg-val)) r
  ;;   (setf cval (logand val #x7))))
  ;;
(defclass rv-temp-reg (rv-reg) ())
(defclass rv-save-reg (rv-reg) ())
(defclass rv-arg-reg (rv-reg) ())

        ;;;; Base (I) register class definitions  ;;;;

(defclass rv-base-reg (rv-reg) ())

(defclass rv-e-reg (rv-base-reg) ())

(defclass rv-cb-reg (rv-e-reg rv-c-reg) ())


;; I might be going a bit overboard here, but it might be useful in future.

(defclass rv-base-temp-reg (rv-base-reg rv-temp-reg) ())
(defclass rv-base-save-reg (rv-base-reg rv-save-reg) ())
(defclass rv-base-arg-reg (rv-base-reg rv-arg-reg) ())

(defclass rv-e-temp-reg (rv-e-reg rv-base-temp-reg) ())

(defclass rv-cb-save-reg (rv-cb-reg rv-base-save-reg) ())
(defclass rv-cb-arg-reg (rv-cb-reg rv-base-arg-reg) ())


        ;;;; Helper macros ;;;;

(defmacro def-reg (name type val &optional (docum nil) )
  `(defconstant ,name (make-instance ,type :val ,val) ,docum))

(defmacro def-multiple-registers (&rest constant-lists)
  `(progn
     ,@(loop for i in constant-lists collect
             `(def-reg ,@i))))

;; Bind base module register names to their correspoinding types and values
;; (def-multiple-constants
(def-multiple-registers
  ;; (zero 'rv-e-reg 0 "Zero register")
  ;; (ra 'rv-e-reg 1 "Return address")
  ;; (sp 'rv-e-reg 2 "Stack pointer")
  ;; (gp 'rv-e-reg 3 "Global pointer") ;- points to start of static region
  ;; (tp 'rv-e-reg 4 "Thread pointer")
  ;; (fp 'rv-e-reg 8 "Frame pointer/Saved Register 0")

  ;; Basic regsiter names
  (x0 'rv-e-reg 0 "Zero register") (x1 'rv-e-reg 1 "Return address")
  (x2 'rv-e-reg 2 "Stack pointer") (x3 'rv-e-reg 3 "Global pointer")
  (x4 'rv-e-reg 4 "Thread pointer")

  (x5 'rv-e-temp-reg 5) (x6 'rv-e-temp-reg 6) (x7 'rv-e-temp-reg 7)

  (x8 'rv-cb-save-reg 8 "Saved register 0/frame pointer")
  (x9 'rv-cb-save-reg 9) (x10 'rv-cb-arg-reg 10)

  (x11 'rv-cb-arg-reg 11) (x12 'rv-cb-arg-reg 12) (x13 'rv-cb-arg-reg 13)
  (x14 'rv-cb-arg-reg 14) (x15 'rv-cb-arg-reg 15)

  (x16 'rv-base-arg-reg 16) (x17 'rv-base-arg-reg 17)
  (x18 'rv-base-save-reg 18) (x19 'rv-base-save-reg 19)
  (x20 'rv-base-save-reg 20) (x21 'rv-base-save-reg 21)
  (x22 'rv-base-save-reg 22) (x23 'rv-base-save-reg 23)
  (x24 'rv-base-save-reg 24) (x25 'rv-base-save-reg 25)
  (x26 'rv-base-save-reg 26) (x27 'rv-base-save-reg 27)
 
  (x28 'rv-base-temp-reg 28) (x29 'rv-base-temp-reg 29)
  (x30 'rv-base-temp-reg 30) (x31 'rv-base-temp-reg 31)

  ;; Argument register aliases
  ;; (a0 'rv-cb-arg-reg 10) (a1 'rv-cb-arg-reg 11) (a2 'rv-cb-arg-reg 12)
  ;; (a3 'rv-cb-arg-reg 13) (a4 'rv-cb-arg-reg 14) (a5 'rv-cb-arg-reg 15)

  ;; (a6 'rv-base-arg-reg 16) (a7 'rv-base-arg-reg 17)

  ;; ;; Saved register aliases
  ;; (s0 'rv-cb-save-reg 8 "Saved register 0/frame pointer")
  ;; (s1 'rv-cb-save-reg 9 "Saved register 1")
  ;; (s2 'rv-base-save-reg 18) (s3 'rv-base-save-reg 19) (s4 'rv-base-save-reg 20)
  ;; (s5 'rv-base-save-reg 21) (s6 'rv-base-save-reg 22) (s7 'rv-base-save-reg 23)
  ;; (s8 'rv-base-save-reg 24) (s9 'rv-base-save-reg 25) (s10 'rv-base-save-reg 26)
  ;; (s11 'rv-base-save-reg 27)

  ;; ;; Temporary regsister aliases
  ;; (t0 'rv-e-temp-reg 5) (t1 'rv-e-temp-reg 6) (t2 'rv-e-temp-reg 7)

  ;; (t3 'rv-base-temp-reg 28) (t4 'rv-base-temp-reg 29) (t5 'rv-base-temp-reg 30)
  ;; (t6 'rv-base-temp-reg 31)

  ;; lrv specific register names
  ;; (r0 'rv-e-reg 0) (r1 'rv-e-reg 1) (r2 'rv-e-reg 2) (r3 'rv-e-reg 3)
  ;; (r4 'rv-e-reg 4)

  ;; (r5 'rv-e-temp-reg 5) (r6 'rv-e-temp-reg 6) (r7 'rv-e-temp-reg 7)

  ;; (r8 'rv-cb-save-reg 8) (r9 'rv-cb-save-reg 9) (r10 'rv-cb-arg-reg 10)

  ;; (r11 'rv-cb-arg-reg 11) (r12 'rv-cb-arg-reg 12) (r13 'rv-cb-arg-reg 13)
  ;; (r14 'rv-cb-arg-reg 14) (r15 'rv-cb-arg-reg 15)

  ;; (r16 'rv-base-arg-reg 16) (r17 'rv-base-arg-reg 17)
  ;; (r18 'rv-base-save-reg 18) (r19 'rv-base-save-reg 19)
  ;; (r20 'rv-base-save-reg 20) (r21 'rv-base-save-reg 21)
  ;; (r22 'rv-base-save-reg 22) (r23 'rv-base-save-reg 23)
  ;; (r24 'rv-base-save-reg 24) (r25 'rv-base-save-reg 25)
  ;; (r26 'rv-base-save-reg 26) (r27 'rv-base-save-reg 27)

  ;; (r28 'rv-base-temp-reg 28) (r29 'rv-base-temp-reg 29)
  ;; (r30 'rv-base-temp-reg 30) (r31 'rv-base-temp-reg 31)
  )

(def-multiple-constants ;; define aliases
  (zero x0) (ra x1) (sp x2) (gp x3) (tp x4)
  (fp x8 "Saved register 0/frame pointer")
  ;; Argument register aliases
  (a0 x10) (a1 x11) (a2 x12) (a3 x13) (a4 x14) (a5 x15) (a6 x16) (a7 x17)
  ;; Saved register aliases
  (s0 x8 "Saved register 0/frame pointer") (s1 x9) (s2 x18) (s3 x19) (s4 x20)
  (s5 x21) (s6 x22) (s7 x23) (s8 x24) (s9 x25) (s10 x26) (s11 x27)
  ;; Temporary regsister aliases
  (t0 x5) (t1 x6) (t2 x7) (t3 x28) (t4 x29) (t5 x30) (t6 x31)
  ;; lrv specific register names
  (r0 x0) (r1 x1) (r2 x2) (r3 x3) (r4 x4) (r5 x5) (r6 x6) (r7 x7) (r8 x8) (r9 x9)
  (r10 x10) (r11 x11) (r12 x12) (r13 x13) (r14 x14) (r15 x15) (r16 x16) (r17 x17)
  (r18 x18) (r19 x19) (r20 x20) (r21 x21) (r22 x22) (r23 x23) (r24 x24) (r25 x25)
  (r26 x26) (r27 x27) (r28 x28) (r29 x29) (r30 x30) (r31 x31)
)


;; TODO: floating point classes

(defclass rv-float-reg (rv-reg) ())

(defclass rv-cf-reg (rv-float-reg rv-c-reg)
  ;; ((cf-reg-value :type '(unsigned-byte 3) :reader get-cf-reg-val :initarg :cfval))
  () )

(defclass rv-f-temp-reg (rv-float-reg rv-temp-reg) ())
(defclass rv-f-save-reg (rv-float-reg rv-save-reg) ())
(defclass rv-f-arg-reg (rv-float-reg rv-arg-reg) ())


(defclass rv-cf-save-reg (rv-cf-reg rv-f-save-reg) ())
(defclass rv-cf-arg-reg (rv-cf-reg rv-f-arg-reg) ())

;; Bind floating point register names to their correspoinding values
;; (def-multiple-constants
(def-multiple-registers
  (f0 'rv-f-temp-reg 0) (f1 'rv-f-temp-reg 1) (f2 'rv-f-temp-reg 2)
  (f3 'rv-f-temp-reg 3) (f4 'rv-f-temp-reg 4) (f5 'rv-f-temp-reg 5)
  (f6 'rv-f-temp-reg 6) (f7 'rv-f-temp-reg 7)
  (f8 'rv-cf-save-reg 8) (f9 'rv-cf-save-reg 9)
  (f10 'rv-cf-arg-reg 10) (f11 'rv-cf-arg-reg 11) (f12 'rv-cf-arg-reg 12)
  (f13 'rv-cf-arg-reg 13) (f14 'rv-cf-arg-reg 14) (f15 'rv-cf-arg-reg 15)
  (f16 'rv-f-arg-reg 16) (f17 'rv-f-arg-reg 17)
  (f18 'rv-f-save-reg 18) (f19 'rv-f-save-reg 19) (f20 'rv-f-save-reg 20)
  (f21 'rv-f-save-reg 21) (f22 'rv-f-save-reg 22) (f23 'rv-f-save-reg 23)
  (f24 'rv-f-save-reg 24) (f25 'rv-f-save-reg 25) (f26 'rv-f-save-reg 26)
  (f27 'rv-f-save-reg 27)
  (f28 'rv-f-temp-reg 28) (f29 'rv-f-temp-reg 29) (f30 'rv-f-temp-reg 30)
  (f31 'rv-f-temp-reg 31)

  ;; (ft0 'rv-f-temp-reg 0) (ft1 'rv-f-temp-reg 1) (ft2 'rv-f-temp-reg 2)
  ;; (ft3 'rv-f-temp-reg 3) (ft4 'rv-f-temp-reg 4) (ft5 'rv-f-temp-reg 5)
  ;; (ft6 'rv-f-temp-reg 6) (ft7 'rv-f-temp-reg 7) (ft8 'rv-f-temp-reg 28)
  ;; (ft9 'rv-f-temp-reg 29) (ft10 'rv-f-temp-reg 30) (ft11 'rv-f-temp-reg 31)

  ;; (fa0 'rv-cf-arg-reg 10) (fa1 'rv-cf-arg-reg 11) (fa2 'rv-cf-arg-reg 12)
  ;; (fa3 'rv-cf-arg-reg 13) (fa4 'rv-cf-arg-reg 14) (fa5 'rv-cf-arg-reg 15)
  ;; (fa6 'rv-f-arg-reg 16) (fa7 'rv-f-arg-reg 17)

  ;; (fs0 'rv-cf-save-reg 8) (fs1 'rv-cf-save-reg 9)
  ;; (fs2 'rv-f-save-reg 18) (fs3 'rv-f-save-reg 19) (fs4 'rv-f-save-reg 20)
  ;; (fs5 'rv-f-save-reg 21) (fs6 'rv-f-save-reg 22) (fs7 'rv-f-save-reg 23)
  ;; (fs8 'rv-f-save-reg 24) (fs9 'rv-f-save-reg 25) (fs10 'rv-f-save-reg 26)
  ;; (fs11 'rv-f-save-reg 27)
  )

(def-multiple-constants ;; define aliases
    ;; temporary floating point registers
  (ft0 f0) (ft1 f1) (ft2 f2) (ft3 f3) (ft4 f4) (ft5 f5) (ft6 f6) (ft7 f7)
  (ft8 f28) (ft9 f29) (ft10 f30) (ft11 f31)
    ;; argument floating point registers
  (fa0 f10) (fa1 f11) (fa2 f12) (fa3 f13) (fa4 f14) (fa5 f15) (fa6 f16) (fa7 f17)
    ;; saved floating point registers
  (fs0 f8) (fs1 f9)(fs2 f18) (fs3 f19) (fs4 f20) (fs5 f21) (fs6 f22) (fs7 f23)
  (fs8 f24) (fs9 f25) (fs10 f26) (fs11 f27)
    )

(defun export-e-regs ()
  "Export names of the E module base registers (x0-x15) and their aliases"
  (export '(
            zero ra sp gp tp fp
            x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15
            a0 a1 a2 a3 a4 a5
            s0 s1
            t0 t1 t2
            r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15)))

(defun export-i-regs ()
  "Export the names of the Base (I) module registers and thier aliases"
  (export-e-regs) ;; Export defined classes and names for base registers 1-15
 
  (export '( ;; export the rest of the base registers names
            x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31
            a6 a7
            s2 s3 s4 s5 s6 s7 s8 s9 s10 s11
            t3 t4 t5 t6
            r16 r17 r18 r19 r20 r21 r22 r23 r24 r25 r26 r27 r28 r29 r30 r31)))


(defun export-fp-regs ()
  "Export the floating point classes, register names and register aliases"
  (export '(
            rv-f-reg rv-cf-reg rv-f-arg-reg rv-f-save-reg
            rv-f-temp-reg rv-cf-arg-reg rv-cf-save-reg

            f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14
            f15 f16 f17 f18 f19 f20 f21 f22 f23 f24 f25 f26 f27 f28 f29 f30 f31

            ft0 ft1 ft2 ft3 ft4 ft5 ft6 ft7 ft8 ft9 ft10 ft11

            fa0 fa1 fa2 fa3 fa4 fa5 fa6 fa7

            fs0 fs1 fs2 fs3 fs4 fs5 fs6 fs7 fs8 fs9 fs10 fs11
   ))
  )

;; (shadow zero ra sp gp tp s0 fp s1 )
;; TODO: Make this choose which register selection to export depending on
;; the modules implemented in the processor
(export-i-regs)
