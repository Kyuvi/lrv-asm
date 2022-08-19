

(in-package "RVASM-UTILS")

(defun align-gen ()
  (if *compressed-instructions*
      (align 2)
      (align 4)))


        ;;;; labels ;;;;

(defun set-addressed-label (name address &optional (env *env*))
  "(set-addressed-label name address [env])
   Set label 'name' to address 'address'.
   env defaults to *env*."
  (env-set-label env name address))

;; (defun set-addressed-label (name address &optional (env *env*))
;;   (env-set-label env label address))

(defun make-end-label (lbl)
  (cond ((keywordp lbl) (keyword-append lbl '-end))
        ((symbolp lbl) (symbol-append lbl '-end))
        (t (error "make-end-label: ~a not a symbol or keyword" lbl))))

(defun weak-label (name backup &key (ofst 0) (env *env*))
  ;; needs to be used before the backup is defined or
  ;; backup needs to be defined after useful name labels
  (assert (not (null env)))
  ;; (pure-delay name (ofst) ;; didn't really work as nearly everything uses delay
  (delay name (ofst)
    (+ ofst (cl::or (env-find-label env name)
                      (progn (warn "using label ~a instead of ~a" backup name)
                             (label backup))))
    ;; (+ ofst (if (env-find-label env name)
    ;;             (env-find-label env name)
    ;;             (progn (warn "using label ~a instead of ~a" backup name)
    ;;                    (label backup))))
)
  )

        ;;;; Data types ;;;;

(defun var-rv (name val &optional (type :vait))
  (set-label name)
  (case type
    ((cl:or :byte :b) (emit-byte val)); (align-gen))
    ((cl:or :jait :j) (emit-jait val)); (align-gen))
    ((cl:or :vait :v) (emit-vait val)); (align-gen))
    ((cl:or :zait :z) (emit-zait val)); (align-gen))
    ((cl:or :yait :y) (emit-yait val)); (align-gen))
    (t (error "named-emit: Unknown type ~a." type))
  ))


(defmacro multiple-var-rv (&rest var-lists)
  `(progn
     ,@(loop for in in var-lists collect
             `(var-rv ,@i)))
  )


;;TODO: unsignied?
;;NOTE: align changes based on inclusion of compressed instruction set
;; (defun named-emit (name type &rest numz)
(defun svec-rv (name type &rest numz)
  (set-label name)
  (case type
    ((cl:or :byte :b) (apply #'emit-byte numz)); (align-gen))
    ((cl:or :jait :j) (apply #'emit-jait numz)); (align-gen))
    ((cl:or :vait :v) (apply #'emit-vait numz)); (align-gen))
    ((cl:or :zait :z) (apply #'emit-zait numz)); (align-gen))
    ((cl:or :yait :y) (apply #'emit-yait numz)); (align-gen))
    (t (error "svec-rv: Unknown type ~a." type))
    )
  ;; (align-gen)
  )

(defun str-rv (name string &optional (encoding :ascii))
  "(str-rv name string [encoding-key])
   Emit a string (vector) into the environment.
   Encoding-key can be one of :ascii, :ucs2, :usb-ucs, :cl
   Encoding-key defaults to :ascii"
 ;; (dolist (byte (map 'list #'char-code
  ;; (apply #'emit-byte (map 'list #'char-code string)))
  ;; (let ((str-list (map 'list #'char-code string)))
  (let ((str-list (string-bytes-list string encoding)))
    ;; (set-label name)
    ;; (apply #'emit-byte
    ;;        (if null-term
    ;;            (append str-list '(0))
    ;;            str-list))
    (apply #'svec-rv name :byte
           ;; (if null-term
               ;; (append str-list '(0))
               str-list))
  ;; (align-gen)
  )
;; )

(defun ascis (name string)
  "Emit a string (vector) of ascii bytes into the enviroment"
  ;; (if (every #'(lambda (x) (< (char-code x) 127)) string)
      (str-rv name string) ; nil)
      ;; (error "Some characters in ~a are not ascii" string)))
  )
 
(defun asciz (name o-str)
  "Emit a string (vector) of ascii bytes terminated by a '0' byte into the enviroment"
  ;; (if (every #'(lambda (x) (< (char-code x) 127)) string)
      (str-rv name (concatenate 'string o-str (string #\null)))
      ;; (error "Some characters in ~a are not ascii" string)))
  )


(defun encode-num (num)
  ;; (loop for x upfrom 8 by 4)
  ;; (let ((num-size 8))
  (labels ((encode-loop (num1 numsize)
             (if (typep num1 `(unsigned-byte ,numsize))
                 (loop for i from 0 upto (- (/ numsize 8) 1) collect (get-byte i num1))
                 ;; (begin (setf num-size (* 2 num-size))
                 (encode-loop num1 (* 2 numsize)))))
    (encode-loop num 8)))
;; )

;; place end-name and len type at end
;;TODO: unsignied?
(defun vec-rv (name type &rest numz)
  (let ((end-label
          (make-end-label name))
        ;; (if (symbolp name)
        ;;     (symbol (concatenate 'string (string name) "-END"))
        ;;     (intern (string-upcase
        ;;              (concatenate 'string (string name) "-END")) "KEYWORD")))
        (vec-length (length numz))
        (type-vec #(:byte :jait :vait :zait :yait))
        )
    (if (not
         (typep vec-length '(unsigned-byte 16))
         ;; (typep vec-length '(unsigned-byte 29))
       ;; (typep vec-length '(unsigned-byte 28))
         )
        (error "Vector ~a too large" name)
        (begin
         (set-label name)
         (case type
           (:byte (apply #'emit-byte numz)); (align-gen))
           (:jait (apply #'emit-jait numz)); (align-gen))
           (:vait (apply #'emit-vait numz)); (align-gen))
           (:zait (apply #'emit-zait numz)); (align-gen))
           (:yait (apply #'emit-yait numz)); (align-gen))
           )
         (set-label end-label)
         (emit-byte (position type type-vec))
         ;; (emit-jait vec-length)
         ;; (emit-vait vec-length)
         ;; (emit-vait (logior (ash (position type type-vec) 29) vec-length))
         ;; (align-gen)
  ))))

;;TODO: unsignied?
;; (defun srec-rv (name cell-size &rest var-pairs)
(defun srec-rv (name &rest vars) ;; vars = (name value &optional cell-size-keyword)
  (flet ((create-slot (struct-name slot-name)
           ;; (intern (concatenate 'string
                                ;; (string struct-name) "." (string slot-name)))
           (symbol-append struct-name '|.| slot-name)
           ))
    (set-label name)
    (loop for i in vars do
      (cond ((= (length i) 3)
             (svec-rv (set-label (create-slot name (car i)))
                      (caddr i) (cadr i)))
            ;; (case (caddr i)
            ;;   ((cl:or :byte :b)
            ;;    (set-label (create-slot name (car i)))
            ;;    (emit-byte (cadr i)))
            ;;   ((cl:or :jait :j)
            ;;    (set-label (create-slot name (car i)))
            ;;    (emit-jait (cadr i)))
            ;;   ((cl:or :vait :v)
            ;;    (set-label (create-slot name (car i)))
            ;;    (emit-vait (cadr i)))))
            ((= (length i) 2)
             (svec-rv (set-label (create-slot name (car i)))
                      :vait (cadr i)))
            ;; (set-label (create-slot name (car i)))
            ;; (emit-vait (cadr i)))
          (t (error "srec-rv: ~a is not valid srec-rv variable" i))))
))


(defstruct rv-srec (vars)) ;; :type list))

(defmacro def-srec-rv (name &rest vars)
  (let* (; (get-name (symbol-append 'get- name '-offset))
         ;; (get-name (symbol-append 'srec- name '-offset))
         (get-name (symbol-append name '-offset))
         ;; (srec-name (symbol-append name '-rv-srec))
         (slot-list (mapcar #'(lambda (x) (if (cl:not (listp x)) x (car x)))
                            vars))
         (type-list (mapcar #'(lambda (x)
                                (if (cl:or (cl:not (listp x))
                                           (cl:not (keywordp (car (last x)))))
                                                   :vait
                                                   (car (last x))))
                            vars))
         (size-list (mapcar
                     #'(lambda (x)
                         (case x
                           ((cl:or :b :byte ) 1)
                           ((cl:or :j :jait ) 2)
                           ((cl:or :v :vait ) 4)
                           (t (error "def-srec-rv: ~a is not a valid risc-v size"
                                     x))))
                              type-list))
         (offset-list (cons 0 (butlast
                               (reverse (maplist #'(lambda (x)
                                                     (apply #'+  x ))
                                                 (reverse size-list))))))
         (keyword-list (mapcar #'(lambda (x) (intern (string x) "KEYWORD"))
         ;; (keyword-list (mapcar #'(lambda (x) (intern (symbol-name x) "KEYWORD"))
                               slot-list))
         (offset-assoc (pairlis keyword-list offset-list) ))
    `(progn
      (defconstant ,name (make-rv-srec :vars ',vars))
      (defun ,get-name (key)  ;; TODO: export?
        (assert (member key ,keyword-list) ()
                "~a not a valid slot key for simple risc-v record ~a. ~%~
                Available slots are ~a"
                ;; key ,srec-name)
                key ,name ,keyword-list)
        (cdr (assoc key ',offset-assoc)))
      ))
  )




;; TODO: change to init-srec-rv?
(defun emit-srec-rv (name srec-type)
  (assert (rv-srec-p srec-type) ()
          "emit-srec-rv: ~a is not a valid simple risc-v record." srec-type)
  (srec-rv name (rv-srec-vars srec-type))
  ;; (srec-rv name srec-type)
  )

(defmacro with-label (label &body body)
  (when (and (listp label) (eql (first label) 'quote))
    (warn "Quoted label name ~a, probably not what you intended" label))
    `(progn (set-label ',label) ,@body))

(defmacro usoro (name &body body)
  `(progn
     (set-label ',name)
     (let ((*env* (make-instance 'local-env :parent *env*)))
       ,@body)))

        ;;;; miscellenous utilites ;;;;

(defun lnot-imm (x imm)
  "Get the 'imm' bits of the logical-not of a number 'x'"
  (bits (lognot x) (- imm 1) 0))
  ;; (bits (tcv (lognot x)) (- imm 1) 0))

(defun lnotb (x)
  "logical not byte
   The lSB byte value of the logical-not of 'x'"
  (lnot-imm x 8))

(defun lnotj (x)
  "logical not jyte
   The lSB jyte value of the logical-not of 'x'"
  (lnot-imm x 16))

(defun lnotv (x)
  "logical not vyte
   The lSB vyte value of the logical-not of 'x'"
  (lnot-imm x 32))
  ;; (tcv (lognot x)))

(defun lnotz (x)
  "logical not zyte
   The lSB zyte value of the logical-not of 'x'"
  (lnot-imm x 64))

(defun bitsv (num)
  "Get the vait bits of 'num'"
  (bits num 31 0))

(defun bitsj (num)
  "Get the jait bits of 'num'"
  (bits num 15 0))

(defun bitsb (num)
  "Get the byte bits of 'num'"
  (bits num 7 0))

(defun jlsb (num)
  "Jait least significant bit: Get the least significant byte of a jait"
  (assert (typep num '(unsigned-byte 16)) () "jlsb: Unrecognised value ~a" num)
  (logand num #xFF ))

(defun jmsb (num)
  "Jait most significant bit: Get the most significant byte of a jait"
  (assert (typep num '(unsigned-byte 16)) () "jmsb: Unrecognised value ~a" num)
  (logand (ash num -8) #xFF ))
