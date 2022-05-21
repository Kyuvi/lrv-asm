
(defpackage "CLRV-UTILS"
  (:documentation "General Common Lisp procedures and macros that are useful in a
                   RISC-V assembly context")
  (:nicknames :clrv)
  ;; (:use :cl )
  (:export
           #:export-mutiple-constants #:def-multiple-constants
           #:ascdr #:def-keyword-assoc
           #:unintern-list #:unexport-list
           #:symbol-append #:keyword-append
           #:hash-create #:hash
           #:string-bytes-list
           #:tcomp-int #:tcv #:tcz
   ))

(in-package "CLRV-UTILS")

(defmacro export-mutiple-constants (&rest constant-lists)
  "(export-mutiple-constants (&rest constant-lists))
   Define multiple constants and export them"
  (let ((name-list (loop for i in constant-lists collect (car i))))
    `(progn
       ,@(loop for i in constant-lists collect
               `(defconstant ,@i))
       (export ',name-list)))
       )
      ;; (export '(name-list))))
      ;; `(export ',name-list)))
  ;; )

;; (export-mutiple-constants (test #x4 "comment") (v 2))

(defmacro def-multiple-constants (&rest constant-lists)
  "(defconsts (&rest constant-lists))
   Define multiple constants."
  `(progn
     ,@(loop for i in constant-lists collect
             `(defconstant ,@i))))

(defun ascdr (obj alist)
  (cdr (assoc obj alist))

(defmacro def-keyword-assoc
         (name (alist &optional (error-string "is not a valid keyword")) )
  (let ((error-output
          (concatenate 'string (string-downcase name) ": ~a " error-string)))
    `(defun ,name (kii)
       ;; (let* ((kii-assoc ,alist)
       ;;        (kii-pair (assoc kii kii-assoc)))
         ;; (assert kii-pair () ,error-ouput kii)
         ;; (cdr kii-pair)
       (let (kii-cdr (cdr (assoc kii ,alist)))
         (assert kii-cdr () ,error-ouput kii)
         kii-cdr

         ;; (error ,error-output kii)
         )))))

;; (defmacro with-alist
;; ((kii ret-nam alist &optional (error-string "is not a recognised keyword")
;; &body body)

;;   )


(defmacro unintern-list (symbol-list );; &optional (pckg *package*))
  "Unintern all items in 'symbol-list' from their original package"
  `(progn
     ,@(loop for i in symbol-list collect
             `(unintern ',i (symbol-package ',i) ))) )

(defmacro unexport-list (symbol-list );; &optional (pckg *package*))
  "Unexport all items in 'symbol-list' from their original package"
  `(progn
     ,@(loop for i in symbol-list collect
             `(unexport ',i (symbol-package ',i) ))) )

(defun symbol-append (&rest syms)
  (intern (apply #'concatenate 'string (mapcar #'string syms))))
  ;; (make-symbol (apply #'concatenate 'string (mapcar #'string syms))))

(defun keyword-append (&rest syms)
  (intern (apply #'concatenate 'string (mapcar #'string syms)) "KEYWORD"))

;; (defun symbol-append (&rest syms &key (key t))
;;   (if :key
;;   (intern (apply #'concatenate 'string (mapcar #'string syms)))
;;   (intern (apply #'concatenate 'string (mapcar #'string syms)) "KEYWORD")
;; ))

(defun hash-create (pairs &key (test #'equal))
  "Create a hash table with limited syntax:
     (hash `((\"name\" \"andrew\") (\"city\" \"santa cruz\")))
   which would otherwise be:
     (let ((hash (make-hash-table :test #'equal)))
       (setf (gethash \"name\" hash) \"andrew\")
       (setf (gethash \"city\" hash) \"santa cruz\")
       hash)
   yuck city."
  (let ((hash (make-hash-table :test test)))
    (dolist (pair pairs)
      (setf (gethash (car pair) hash) (cadr pair)))
    hash))

(defmacro hash (&rest pairs)
  "Extends hash-create syntax to make it nicer."
  `(hash-create
    (list ,@(loop for pair in pairs collect
                                    (list 'list (car pair) (cadr pair))))))

(defun string-bytes-list (string &optional (encoding :cl))
  (let ((str-list (map 'list #'char-code string)))
  (case encoding
    ((cl:or :a :ascii)
     (assert (every #'(lambda (x) (typep x '(unsigned-byte 7))) str-list) ()
             "char-code-list: Some characters in ~a are not ascii" string)
     str-list)
     ((cl:or :cl :norm) str-list) ;; TODO: needs to return bytes
     ;; ((cl:or :utf :utf-16)
     ;;  (apply #'concatenate 'list
     ;;         (mapcar
     ;;          #'(lambda (x) (if (< x 127)
     ;;                            (list (logand (ash x -8) #xFF)
     ;;                                  (logand x #xFF))
     ;;                           (list x))
     ;;          str-list))))
     ;; ((cl:or :utf32 :utf-32)
     ;; ;; TODO: full 32bit unicode for usb?
     ;; ;;  see logan output on analyser/wireshark
     ;; )
     ((cl:or :usb-ucs)
      ;; (apply #'concatenate 'list
      (apply #'concatenate 'list
             (mapcar
              #'(lambda (x) (list ;; for usb
                             (logand x #xFF) ;; lsb first
                             (logand (ash x -8) #xFF) ;; msb second
                             ))
              str-list)))
     ((cl:or :ucs2 :ucs)
      (apply #'concatenate 'list
             (mapcar
              #'(lambda (x) (list (logand (ash x -8) #xFF)
                                  (logand x #xFF)
                                  ))
              str-list)))

     )))


 ;; NOTE: watch does it need bit-length or should it 'cut' num?
(defun twos-complement-int (num bit-length)
  "Return the twos complement of the number 'num' within the range of 'bit-length"
  (assert (cl:and (typep num 'integer) (<= (integer-length num) bit-length)) ()
                 "twos-complement-int: ~~
                  ~a is either not an integer or out of the ~a bit range"
                 num bit-length )
  (let ((sign-bit (ldb-test (byte 1 (- bit-length 1)) num)))
    (cond
            ;; ((cl:not (typep num 'integer))
            ;;  (error "~A  Value is not rational: " num))
            ;; ((> len bit-length)
            ;;   (error "~A  Value out of range: " num))
            ((cl:and (null sign-bit) (plusp num))
            ;; positive numbers are ok if less than register-length
             num)
            ((cl:and sign-bit (plusp num))
            ;; if sign bit is set for positive numbers, complement
             (- (logxor #xffffffff (- num 1))))
            ((cl:and (null sign-bit) (minusp num))
            ;; large negative numbers whose complement bits are higher than the
            ;; MSB of the register-length in common lisp number representation
            ;; need to be complemented as well by adding them to max bit-length + 1
             (+ (expt 2 bit-length) num))
            ((cl:and sign-bit (minusp num))
            ;; negative numbers smaller than register length
            ;; are already complemented properly
             num)
             (t (error "twos-complement-int: Error with number ~A." num)))))

(defun tcv (num)
  "Return the 32 bit twos complement of num"
  (twos-complement-int num 32))

(defun tcz (num)
  "Return the 64 bit twos complement of num"
  (twos-complement-int num 64))

        ;;;; Dispatch read macros for twos complement numbers ;;;;

;; RISC-V assemblers take only positive whole number hexidecimal and binary,
;; (at least in the integer modules). These reader macros convert them from the
;; two complement representation of binary and hexidecimal (for now) to
;; the decimal positive or negative value based on the given register length
;; this (hopefully) makes it easier to optimize certain fuctions
;; (pseudo instructions) that take register length values
;; (i.e. li, la, lb, lw etc).
;; it also makes the code more in line with mainstream assemblers/compilers.

(defun ignore-numarg (sub-char numarg)
  (when numarg
    (warn "A numeric argument was ignored in #~W~A." numarg sub-char)))

(defun twos-complement-sharp (stream sub-char bit-length base )
  "Reads a stream of numbers and converts them to the twos complement
   representation based on the bit-length (register length) and the base,
   with the most significant bit representing if the number is negated or not
   (NOTE: has some strange unresolved errors but should be ok in assembly code)"
  ;; #h#y4 gives strange error
  ;; (read-from-string "#h 20") -> 32
  ;; (read-from-string "#h #+foo a b") -> 12
  ;; see SBCL code/sharpm.lisp where this was lifted from
  (let ((res (let ((*read-base* base))
               (read stream t nil t))))
    (let ((len (integer-length res))
          (sign-bit (ldb-test (byte 1 (- bit-length 1)) res)))
      (cond ((cl:not (typep res 'rational))
             (error "#~A (base ~d) Value is not rational: ~S" sub-char base res))
            ((> len bit-length)
              (error "#~A (base ~d) Value out of range: ~S" sub-char base res))
            ((cl:and (null sign-bit) (plusp res))
            ;; positive numbers are ok if less than register-length
             res)
            ((cl:and sign-bit (plusp res))
            ;; if sign bit is set for positive numbers, complement
             (- (logxor #xffffffff (- res 1))))
            ((cl:and (null sign-bit) (minusp res))
            ;; large negative numbers whose complement bits are higher than the
            ;; MSB of the register-length in common lisp number representation
            ;; need to be complemented as well by adding them to max bit-length + 1
             (+ (expt 2 bit-length) res))
            ((cl:and sign-bit (minusp res))
            ;; negative numbers smaller than register length
            ;; are already complemented properly
             res)
             (t
              (error "Error in twos complement sharp (base ~d) #~A , ~S"
                     base sub-char res))
             ))))


(defun sharp-h-32 (stream sub-char numarg)
  "function to read hexidecimal numbers for a 32-bit processor"
  (ignore-numarg sub-char numarg)
    (twos-complement-sharp stream sub-char 32 16))

(defun sharp-y-32 (stream sub-char numarg)
  "function to read binary numbers for a 32-bit processor"
  (ignore-numarg sub-char numarg)
    (twos-complement-sharp stream sub-char 32 2))

(defun sharp-h-64 (stream sub-char numarg)
  "function to read hexidecimal numbers for a 64-bit processor"
  (ignore-numarg sub-char numarg)
    (twos-complement-sharp stream sub-char 64 16))

(defun sharp-y-64 (stream sub-char numarg)
  "function to read binary numbers for a 64-bit processor"
  (ignore-numarg sub-char numarg)
  (twos-complement-sharp stream sub-char 64 2))

;; TODO use correct function depending on register size of processor
;; here only 32 bit are defined
(set-dispatch-macro-character #\# #\h #'sharp-h-32) ;; hexidecimals
(set-dispatch-macro-character #\# #\y #'sharp-y-32) ;; binary
