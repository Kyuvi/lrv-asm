

(in-package "RVASM-UTILS")

(defun align-gen ()
  (if *compressed-instructions*
      (align 2)
      (align 4)))


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
)
  )


;;TODO: unsignied?
;;NOTE: align changes based on inclusion of compressed instruction set
(defun named-emit (name type &rest numz)
  (set-label name)
  (case type
    ((cl:or :byte :b) (apply #'emit-byte numz)); (align-gen))
    ((cl:or :jait :j) (apply #'emit-jait numz)); (align-gen))
    ((cl:or :vait :v) (apply #'emit-vait numz)); (align-gen))
    ((cl:or :zait :z) (apply #'emit-zait numz)); (align-gen))
    ((cl:or :yait :y) (apply #'emit-yait numz)); (align-gen))
    (t (error "named-emit: Unknown type ~a." type))
    )
  ;; (align-gen)
  )

(defun str-rv (name string &optional (nul t))
 ;; (dolist (byte (map 'list #'char-code
  ;; (apply #'emit-byte (map 'list #'char-code string)))
  (set-label name)
  (if nul
      (apply #'emit-byte (append (map 'list #'char-code string) '(0)))
      (apply #'emit-byte (map 'list #'char-code string)))
  ;; (align-gen)
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
;; (defun struct-rv (name cell-size &rest var-pairs)
(defun struct-rv (name &rest vars) ;; vars = (name value &optional cell-size-keyword)
  (set-label name)

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

(defun lnotz (x)
  "logical not zyte
   The lSB zyte value of the logical-not of 'x'"
  (lnot-imm x 64))
