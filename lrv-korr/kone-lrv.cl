
                    ;;;;;;;; Procedures for the Risc-v assembler ;;;;;;;;

(in-package "RVASM")

;; (defvar *pc* (if (boundp '*pc*) *pc* 0))
(define-symbol-macro *pc* (if *env* (env-address *env*) 0))

;; (defun x16 (n)
;;  "Print 16-bit number in hex"
;;   (format nil "~4,'0x" n ))
;;   ;; (printhex 4 n))

;; (defun x32 (n)
;;  "Print 32-bit number in hex"
;;   (format nil "~8,'0x" n ))
;;   ;; (printhex 8 n))

;; (defun printhex (m n)
  ;; (princ "#x")
  ;; (dotimes (j m nil)
  ;;   (let ((d (logand (ash n (* (- j m -1) 4)) #xf)))
  ;;     (princ
  ;;      (code-char (+ d (if (< d 10) (char-code #\0) (char-code #\W))))))))

(defun rv-error (txt &optional (addr *pc*))
  ; (princ "(pc=") (x16 *pc*) (princ ") ") (princ txt) (terpri))
  (error "Error at (pc=#x~4,'0x) ~a"  addr txt))
         ;; (x16 *pc*) txt))
  ;; (error "(pc=#x~a) ~a" (if (boundp 'addr) (x16 addr) (x16 *pc*)) txt))


;; (defvar *reg-assoc* '((zero . 0) (ra . 1) (sp . 2) (gp. 3) (tp . 4) (s0 . 8)
;;     (fp . 8)(s1 . 9)
;;                            ))

;; (defun regnos (sym)
;;   (cdr (assoc sym *reg-assoc*)))

(defun regno (sym)
 "Extract register number
  x0-31 = Normal Register Names
  zero = x0
  ra = Return Address = x1
  sp = Stack Pointer = x2
  gp = Global Pointer = x3
  tp = Thread Pointer = x4
  t0 = Temporary/alternate link register = x5
  t1-2 = Temporaries = x6-7
  s0 = Saved Register = x8
  fp = Frame Pointer = x8
  s1 = Saved Register = x9
  a0-1 = Function arguments/return values = x10-11
  a2-7 = Function arguments = x12-17
  s2-11 = Saved Registers = x18-27
  t3-6 = Temporaries = x28-31
  r0-r31 = Local Register Names
"
  ;; Note: the first (non calculated) register names need to be exported
  (case sym (zero 0) ; zero register
            (ra 1) ; return address
            (sp 2) ; stack pointer
            (gp 3) ; global pointer
            (tp 4) ; thread pointer
            (s0 8) ; saved register
            (fp 8) ; frame pointer
            (s1 9) ; saved register
    (t (let* ((s (string sym))
              (c (char s 0))
              (n (read-from-string (subseq s 1))))
         (case c (#\X (typecase n
                        ((integer 0 31) n)
                        (t (rv-error "An x register needs to be between 0 and 31"))))
                 (#\A (typecase n
                        ((integer 0 7) (+ n 10))
                        (t (rv-error "An a register needs to be between 0 and 7"))))
                 (#\S (typecase n
                        ((integer 2 11) (+ n 16))
                        (t (rv-error "An s register needs to be between 0 and 11"))))
                 (#\T (typecase n
                        ((integer 0 6) (if (<= n 2) (+ n 5) (+ n 25)))
                        (t (rv-error "A t register needs to be between 0 and 6"))))
                 (#\R (typecase n
                        ((integer 0 31) n)
                        (t (rv-error "A r register needs to be between 0 and 31"))))
                 (t (rv-error "Unknown risc-v register"))
                 )))))

; Short 3-bit register
(defun cregp (sym) (<= 8 (regno sym) 15))

(defun cregno (sym) (if (cregp sym)
                        (logand (regno sym) #x7)
                        (rv-error "Invalid compressed register (use x8 .. x15)")))

; Errors

; Pack arguments into bit fields
; this builds the machine code for expressions
(defun build-expr-code (bits &rest args)
  (let ((word 0))
    (mapc #'(lambda (width value)
              (unless (zerop (ash value (- width)))
                (rv-error "Build expression doesn't fit"))
              (setq word (logior (ash word width) value)))
          bits args)
    word))

; 32-bit emit produces a list of two 16 bit numbers
;; (defun build-expr-code* (bits &rest args)
;;   (let ((word (apply #'emit bits args)))
;;     (values
;;      (list (logand word #xffff) (logand (ash word -16) #xffff))
;;      word)
;;     ))

; Test range of immediate values
; Test that x can fit into b bits (2's compliment)
(defun immp (x b)
 "Test that x can fit into b bits (2's compliment)"
  (<= (- (ash 1 (1- b)))
      x
      (1- (ash 1 (1- b)))))
 ;; (typep x '(signed-byte b))
 ;; (= (integer-length x) b))

(defun uimmp (x b)
  "Test that x can fit into b bits (unsigned)"
  ;; (<= 0 x (1- (ash 1 (1+ b))))
  (<= 0 x (1- (ash 1  b)))
 ;; (typep x '(unsigned-byte b))
  )

; Extract bitfield read bit number a of x (a starts from zero)
; or read from bits a to b of x (if b>a then performs (bit x max b))
(defun bits (x a &optional b)
  "Extract bitfield number a from x or read bits b to a of x
   (if b > a performs '(bits max b)')"
  ;; (if b (ldb (byte (- a b) b) x)
  ;;       (ldb (byte 1 a) x )))
  (if b
      (logand (ash x (- b)) (1- (ash 1 (- a b -1))))
      (logand (ash x (- a)) 1)))

;; (defun offset (label) (- label *pc*))

;; (defgeneric offset (labl)
;;   (:method ((labl integer)) (- labl *pc*))
;;   (:method ((labl promise)) (delay "offset" (labl) (offset labl))))

(defun offset (labl) ; &optional adr)
  (let ((addr *pc*) ;; this stores(captures?) the *pc* at that instance
  ;; (let ((adr (if adr adr *pc*)) ;; this stores(captures?) the *pc* at that instance
        ;; (label labl)
        )
    (delay :offset (labl) (- labl addr ))))


        ;;;; miscellenous utilites ;;;;
(defun lnot-imm (x imm)
  "Get the 'imm' bits of the logical not of a number 'x'"
  (bits (lognot x) (- imm 1) 0))

(defun lnotb (x)
  "logical not byte
   The lSB byte value of the logical not of 'x'"
  (lnot-imm x 8))

(defun lnotj (x)
  "logical not jyte
   The lSB jyte value of the logical not of 'x'"
  (lnot-imm x 16))

(defun lnotv (x)
  "logical not vyte
   The lSB vyte value of the logical not of 'x'"
  (lnot-imm x 32))

(defun lnotz (x)
  "logical not zyte
   The lSB zyte value of the logical not of 'x'"
  (lnot-imm x 64))
