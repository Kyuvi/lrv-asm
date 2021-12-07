
                    ;;;;;;;; risc-v assembly environment ;;;;;;;;
(in-package "RVASM")

;; The output file is converted from a vector of bytes into a file,
;; this class contains that final vector and the byte address of the final
;; byte in that the vector

(defclass code-vector ()
  ((code-vector :initarg :code-vector
                :reader env-code-vector
                :initform (make-array 0 :adjustable t :fill-pointer t))
   (address :initarg :address :accessor env-address :initform 0))
  (:documentation "Code-vector class with a code-vector slot containg a vector
   of bytes and an address slot which holds the byte address of the last byte")
  )

;; As most assembly code contains labels to jump to this class provides
;; a (hash) lookup table of the labels/symbols in the code and hopefully
;; the addresses they point to

(defclass symbol-table ()
  ((symbol-table :reader env-symbol-table
                 :initform (make-hash-table :test 'equal)))
  (:documentation
   "symbol-table class with a single symbol-table slot
    containing a hash table for labels (and other symbols?)")
  )

;; Most assembly environments will probably have both a code-vector
;; and a symbol-table so we define a new class with both as super classes
(defclass basic-env (code-vector symbol-table) ())

        ;;;; Delayed evaluation ;;;;

;; The labels might be used before they are defined/set so a system of promise
;; sructures and force functions needs to be set up to evaluate the labels when
;; neccesary


(defvar *lazy-marker* '#:postponed)

(defstruct promise name fun (value *lazy-marker*))

;; Create a new condition class that can temporarily be stored in place of a
;; label address if the address can not be resolved yet
(define-condition resolvable-condition (condition)
  ((state :initform nil :initarg :state :accessor state))
  (:report (lambda (condition stream)
             (format stream "~a" (state condition)))))

(defgeneric force (expression &optional force-p)
  (:documentation "Forces computing the value of a delayed expression"))

(defmethod force ((expression number) &optional force-p)
  (declare (ignore force-p))
  expression)

(defparameter *memoize-promises* t
  "Controls whether fufilled promises are cached. Only useful in abuses
   of the promise mechanism involving special variables. Don't do this")

(defun set-promise-value (promise value)
  (when *memoize-promises*
    (setf (promise-value promise) value))
  value)

;; Returns the promise-value of the "promise" if it exists,
;; or sets the promise-value of the promise to the result of executing the
;; promise-fun of the promise. if not executing promise-fun returns a
;; resolvable condition, then set the state of the resolvable condition
;; to error or signal the condition and return the promise
(defmethod force ((p promise) &optional (error-p t))
" Returns the promise-value of the 'promise' if it exists,
  or sets the promise-value of the promise to the result of executing
  the promise-fun of the promise. if not, executing promise-fun
  returns a resolvable condition, then sets the state of the
  resolvable condition to error or signal the condition and return the promise"
  (if (not (eq (promise-value p) *lazy-marker*))
      (promise-value p)
      (handler-case (set-promise-value p (funcall (promise-fun p)))
        (resolvable-condition (condition)
          (setf (state condition) (cons (promise-name p) (state condition)))
          (funcall (if error-p #'error #'signal) condition)
          p))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-binding (spec)
  ;; if spec is a symbol return a list of the symbol and itself
  ;; if spec is a list of two symbols, return spec
  ;; else raise error
    (etypecase spec
      (symbol (list spec spec))
      ((cons symbol (cons t null)) spec))))

;; Builds an anonymous function/procedure from the dependencies and body
(defmacro forcing (dependencies &body body)
"Builds an anonymous function/procedure from the dependencies and body"
  (let ((bindings (mapcar #'parse-binding dependencies)))
    `((lambda ,(mapcar #'first bindings) ,@body)
      ,@(loop for b in bindings collect `(force ,(second b))))))

;; Creates a new promise and using "force" either resolves the promise instantly
;; or returns the new promise
(defmacro delay (name dependencies &body body)
 "Creates a new promise and using 'force' either resolves the promise instantly
  or returns the new promise"
  `(force
    (make-promise :name ,name :fun (lambda () (forcing ,dependencies ,@body)))
    nil))

(defun resolve-tree (tree)
  (etypecase tree
    (cons (cons (resolve-tree (car tree))
                (resolve-tree (cdr tree))))
    (null tree)
    (integer tree)
    (promise (force tree))))


        ;;;; bits, bytes, jaits, vaits, zaits and yaits ;;;;

;; The instruction are loaded into the vector in a series of bytes (8-bits)
;; Risc-v uses Indus instructions, also known as little endian, this means
;; the 32 or 16 bit instructions are inverted bytewise (in 8-bit chunks) before being
;; stored into memory


(defun test-byte (x)
  ;; (delay :test-byte (x) (etypecase x ((integer 0 255) x))))
  (delay :test-byte (x) (etypecase x ((unsigned-byte 8) x))))

(defun test-jait (x)
  ;; (delay :test-jait (x) (etypecase x ((integer 0 65535) x))))
  (delay :test-jait (x) (etypecase x ((unsigned-byte 16) x))))

(defun test-vait (x)
  ;; (delay :test-vait (x) (etypecase x ((integer 0 #xffffffff) x))))
  (delay :test-vait (x) (etypecase x ((unsigned-byte 32) x))))

(defun test-zait (x)
  ;; (delay :test-zait (x) (etypecase x ((integer 0 #xffffffffffffffff) x))))
  (delay :test-zait (x) (etypecase x ((unsigned-byte 64) x))))

(defun test-yait (x)
  ;; (delay :test-yait (x)
         ;; (etypecase x ((integer 0 #xffffffffffffffffffffffffffffffff) x))))
  (delay :test-yait (x) (etypecase x ((unsigned-byte 128) x))))

;; get a single byte sized chunk of a number
;; (defun get-bait (n val)
;;   (logand #xff (ash val (- (* 8 n ))))
;;   )

;; Extract single byte "n" from a number "val"
;; (byte 0 being the least significant byte)
(defgeneric get-byte (n val)
  (:method (n (val integer)) (ldb (byte 8 (* 8 n)) val))
  (:method (n (val promise)) (delay :bait (val) (get-byte n val))))
  ;; (:method (n (val promise)) (delay :bait (n val) (get-byte n val))))

(defun do-encode (name obj num  test)
  "Build 'num' (+ 1) byte vector of encoded 'obj'
   using name for delay name and test to test fit"
  (coerce (loop for i upto num collect
                               (delay name (obj) (get-byte i (funcall test obj))))
        ;; test needs to be inside delay to ensure testing number
  ;; (coerce (loop for i upto num collect
                               ;; (delay name (obj) (get-byte i obj)))
          'vector))
  ;; (dotimes (x num)
    ;; (delay name (obj) (get-byte x (test obj)) )))


(defun encode-byte (byte &optional (name "byte"))
  (vector (delay name (byte) (get-byte 0 (test-byte byte)))))
  ;; (do-encode (name byte 0 test-byte)))

(defun encode-jait (jait &optional (name "jait"))
  (vector (delay name (jait) (get-byte 0 (test-jait jait)))
          (delay name (jait) (get-byte 1 (test-jait jait)))))
  ;; (do-encode (name jait 1 test-jait)))

(defun encode-vait (vait &optional (name "vait"))
  (vector (delay name (vait) (get-byte 0 (test-vait vait)))
          (delay name (vait) (get-byte 1 (test-vait vait)))
          (delay name (vait) (get-byte 2 (test-vait vait)))
          (delay name (vait) (get-byte 3 (test-vait vait)))))
  ;; (do-encode (name vait 3 test-vait))))

(defun encode-zait (zait &optional (name "zait"))
  (vector (delay name (zait) (get-byte 0 (test-zait zait)))
          (delay name (zait) (get-byte 1 (test-zait zait)))
          (delay name (zait) (get-byte 2 (test-zait zait)))
          (delay name (zait) (get-byte 3 (test-zait zait)))
          (delay name (zait) (get-byte 4 (test-zait zait)))
          (delay name (zait) (get-byte 5 (test-zait zait)))
          (delay name (zait) (get-byte 6 (test-zait zait)))
          (delay name (zait) (get-byte 7 (test-zait zait)))))
  ;; (do-encode (name zait 7 test-zait))))
  ;;
(defun encode-yait (yait &optional (name "yait"))
  ;; (check-type yait (unsigned-byte 128))
  (do-encode name yait 15  #'test-yait))
  ;; (coerce (loop for i upto 15 collect
  ;;                             (delay name (yait) (get-byte i (test-yait yait))))
  ;;         'vector))

(defun join-masks (x y)
  (unless (zerop (logand x y))
    (error "Bitmasks ~a and ~a overlap!" x y))
  (logior x y))






        ;;;; generic functions for environments ;;;;

(defgeneric env-emit (env vector)
  (:documentation "Emit a vector of bytes into the assembly environment."))

(defgeneric env-address (env)
  (:documentation "Returns the current virtual address of the environment."))

(defgeneric (setf env-address) (address env)
  (:documentation "Sets the current virtual address of the environment."))

(defgeneric env-find-label (env symbol)
  (:documentation "Returns the address of a label within the environment."))

(defgeneric env-set-label (env symbol &optional address)
  (:documentation "Sets the address of a label within the environment.
  If not supplied the current address is used."))

(defgeneric env-emit-ins (env vector)
  (:documentation "Emit an instruction into the assembly environment. This is a
    hint, for environments which want to handle instructions specially.")
  (:method (env vector) (env-emit env vector)))

(defgeneric link (env)
  (:documentation "Prepare and return final assembled output vector"))


        ;;;; methods for assembly environmaents ;;;;

(defmethod env-find-label ((env symbol-table) symbol)
  (with-slots (symbol-table) env
    (gethash symbol symbol-table)))

(defmethod env-set-label ((env symbol-table) symbol
                          &optional (address (env-address env)))
  (with-slots (symbol-table) env
    (setf (gethash symbol symbol-table) address)))

;; TODO make env-address accept delayed pc? also delayed labels?
(defmethod env-emit ((env code-vector) bytes)
  (when (> (+ (env-address env) (length bytes)) *max-address*)
    (warn "Content emit of $~x bytes at $~x will overflow address space"
          (length bytes)
          (env-address env)))
  (map nil (lambda (x)
             (unless (typep x '(cl::or (integer 0 255) promise))
             ;; (unless (cl::or (typep x '(integer 0 255)) (typep x 'promise))
               (error "Attempt to emit garbage (~a) at ~x" x (env-address env)))
             (vector-push-extend x (env-code-vector env)))
       bytes)
  (incf (env-address env) (length bytes)))

(defmethod link ((env code-vector))
  (resolve-vector (env-code-vector env)))

(defun resolve-vector (vector)
  (let (problems)
    (prog1 (map 'vector (lambda (x) (handler-case (force x t)
                                      (resolvable-condition (c)
                                        (push (state c) problems)
                                        x)))
                vector)
      (when problems
        (error "Unable to resolve output due to the following:~%~a~%"
               problems)))))


        ;;;; delegated environments ;;;;

;; These seem to be used to "clone" an environment (i.e. a basic-env)
;; that has been instanciated already and then "inject" code
;; into that environment (i.e. for pocedures)
;; it might also add a second (temprorary?) symbol table

(defclass delegate-env ()
  ((parent :reader env-parent :initarg :parent)))

(defmethod env-address ((env delegate-env))
  (env-address (env-parent env)))

(defmethod (setf env-address) (address (env delegate-env))
  (setf (env-address (env-parent env)) address))

(defclass delegate-code-vector (delegate-env) ())

(defmethod env-emit ((env delegate-code-vector) vector)
  (env-emit (env-parent env) vector))

(defclass delegate-symbol-lookup (delegate-env) ())

(defmethod env-find-label ((env delegate-symbol-lookup) symbol)
  (env-find-label (env-parent env) symbol))

(defclass delegate-symbol-definition (delegate-env) ())

;; (defmethod env-set-label ((env delegate-symbol-lookup) symbol
(defmethod env-set-label ((env delegate-symbol-definition) symbol
                          &optional (address (env-address env)))
  (env-set-label (env-parent env) symbol address))

(defclass local-symbol-table (delegate-symbol-lookup symbol-table) ())

(defmethod env-find-label ((env local-symbol-table) symbol)
  (with-slots (symbol-table) env
    (gethash symbol symbol-table (env-find-label (env-parent env) symbol))))

;; Local environmet, the base upon which to build local symbol scopes
;; and special-purpose environments

(defclass local-env (delegate-code-vector local-symbol-table) ())


        ;;;; User interface ;;;;

(defvar *env* nil "Current assembly environment")
(define-symbol-macro *origin* (env-address *env*))

(defun emit (bytes) (env-emit *env* bytes))

(defun emit-byte (&rest bytes)
  (dolist (byte bytes) (env-emit *env* (encode-byte byte))))

(defun emit-jait (&rest jaits)
  (dolist (jait jaits) (env-emit *env* (encode-jait jait))))

(defun emit-vait (&rest vaits)
  (dolist (vait vaits) (env-emit *env* (encode-vait vait))))

(defun emit-zait (&rest zaits)
  (dolist (zait zaits) (env-emit *env* (encode-zait zait))))

(defun emit-yait (&rest yaits)
  (dolist (yait yaits) (env-emit *env* (encode-yait yait))))

(defun advance-to (offset &optional (fill-byte #xff))
  (let ((delta (- offset (env-address *env*))))
    (when (< delta 0)
    (error "Cannot advance to ~x, it is less than the current enviroment address (~x)"
           offset (env-address *env*)))
    (env-emit *env* (make-array delta :initial-element fill-byte))))

(defun align (alignment &optional (fill-byte #xff))
  (advance-to (* alignment (ceiling (env-address *env*) alignment)) fill-byte))

;; Within an assembly environment, either returns the label position
;; or a promise with the same name as the label
(defun label (name &key (offset 0) (env *env*))
  "Within an  assembly environment, either returns the label position
  or a promise with the same name as the label"
  (assert (not (null env)))
  (delay name (offset)
         (+ offset (cl::or (env-find-label env name)
                       (error 'resolvable-condition
                              :state (format nil "Label ~a is undefined" name))))))


(defun set-label (name &optional (env *env*))
  (env-set-label env name)
  name)

(defun label-difference (start-name end-name)
  (let ((start (label start-name))
        (end (label end-name)))
    (delay :label-difference (start end)
      (- end start))))



(defmacro with-label (label &body body)
  (when (and (listp label) (eql (first label) 'quote))
    (warn "Quoted label name ~a, probably not what you intended" label)
    `(progn (set-label ',label) ,@body)))

(defmacro usoro (name &body body)
  `(progn
     (set-label ',name)
     (let ((*env* (make-instance 'local-env :parent *env*)))
       ,@body)))

