
(defpackage "DEBUG-UTILS-RV"
  (:documentation "Useful common lisp uitilities when writing an assembly program,
                   But not neccesarily used in the assembly process")
  (:nicknames :durv)
  (:use :cl :rvasm)
  (:export
         #:prn-tcomp #:splnum #:imm #:uimm
   ))

 (use-package :durv)
(in-package :durv)

(defun prn-tcomp (num &optional (len 0))
  "Print the two's complememt binary representation of a number"
  (let* ((ilen (+ (integer-length num) 1))
         (olen (if (> ilen len) ilen len)))
    (format t (concatenate 'string "~" (write-to-string olen) ",'0b")
            ;; use "0" as padding because it is hopefully only needed for positive numbers
            (ldb (byte olen 0) num))))


(defun splnum (x)
  "Split a number into upper 20 bits and lower 12 bits and print them out
   For use with auipc/lui and addi/jalr"
  (let* ((imm20 (logand #xfffff000 x))
         (imm12 (logand #x00000fff x))
         (pimm20 (if (=  (logand imm12 #x800) #x800) (+ imm20 #x1000) imm20 ))
         (total (+ (logand #xfffff000 x) (logand #x00000fff x))))
        ;; (format t "bin ~b ~&imm20=~b, imm12=~b, pimm20=~b, total=~b.~&"
        ;;         x imm20 imm12 pimm20 total)
        ;; (format t "hek ~x ~&imm20=~x, imm12=~x, pimm20=~x,total=~x.~&"
        ;;         x imm20 imm12 pimm20 total)
        ;; (format t "des ~d ~&imm20=~d, imm12=~d, pimm20=~d, total=~d.~&~%"
        ;;         x imm20 imm12 pimm20 total)))
        (format t "bin ~b ~&imm20=~b, imm12=~b, pimm20=~b, total=~b.~&~
                   ~5:*hek ~x ~&imm20=~x, imm12=~x, pimm20=~x,total=~x.~&~
                   ~5:*des ~d ~&imm20=~d, imm12=~d, pimm20=~d, total=~d.~&~&"
                x imm20 imm12 pimm20 total)))

(defun imm (num)
  "Print the minimum and maximum signed numbers that can fit into x number of bits
   using twos complement. Returns a list of the minimum and maximum numbers"
  (let* ((maxm (loop for v from (- num 2) downto 0 sum (expt 2 v)))
         (minm (- (+ 1 maxm))))
    (format t "For a signed integer with a bit length of ~d,~%~
               minimum = ~:d | ~:*#x~:x,~%~
               maximum = ~:d | ~:*#x~:x"
            num minm maxm); minm maxm maxm)
    (list minm maxm)
        ))


(defun uimm (num)
  "Print the minimum and maximum unsigned number that can fit into x number of bits
   Returns the maximum number"
  (let ((maxm (loop for v from (- num 1) downto 0 sum (expt 2 v))))
    ;; (format t "length = ~d minimum = 0, maximum = ~d / #b~b / #x~x" num max max max)
    (format t "For an unsigned integer with a bit length of ~d,~%~
               minimum = 0, ~
               maximum = ~:d | ~:*#x~:x" num maxm); maxm)
    maxm
  ))
