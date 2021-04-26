
                ;;;;;;;; Control and Status Register instructions  ;;;;;;;;


(defpackage "ZICSR-RV"
  (:documentation "Risc-V instructions for the control and status register")
  (:nicknames :csr)
  (:use :cl :rvasm)
  (:export #:csrrw #:csrrs #:csrrc
           #:csrrwi #:csrrsi #:csrrci
           #:csrr #:csrw #:csrs #:csrc
           #:csrwi #:csrsi #:csrci
   ))

(in-package "ZICSR-RV")

        ;;;; register instructions ;;;;

(defun csrrw (rd rs1 csr)
  "(csrrw rd rs1 csr)
   Reads contents of csr (zero extended) into rd and writes contents of rs1 to the csr"
  (csrreg csr rs1 1 rd))

(defun csrrs (rd rs1 csr)
  "(csrrs rd rs1 csr)
   Reads contents of csr (zero extended) into rd and sets the bits in the csr
   that are set in rs1"
  (csrreg csr rs1 2 rd))

(defun csrrc (rd rs1 csr)
  "(csrrc rd rs1 csr)
   Reads contents of csr (zero extended) into rd and clears the bits in the csr
   that are set in rs1"
  (csrreg csr rs1 3 rd))

        ;;;; immediate instructions ;;;;

(defun csrrwi (rd uimm5 csr)
  "(csrrwi rd uimm5 csr)
   Reads contents of csr into rd and writes zero extended immediate 'uimm5' to the csr"
  (csrimm csr uimm5 5 rd))

(defun csrrsi (rd uimm5 csr)
  "(csrrsi rd uimm5 csr)
   Reads contents of csr (zero extended) into rd and sets the bits in the csr
   that are set in the immediate 'uimm5'"
  (csrimm csr uimm5 6 rd))

(defun csrrci (rd uimm5 csr)
  "(csrrci rd uimm5 csr)
   Reads contents of csr (zero extended) into rd and clears the bits in the csr
   that are set in the immediate 'uimm5'"
  (csrimm csr uimm5 7 rd))

        ;;;; derived register instructions ;;;;

(defun csrr (rd csr)
  "(csrr rd csr)
    Reads contents of csr (zero extended) to rd"
  (csrrs rd 'x0 csr ))

(defun csrw (rs1 csr)
 "(csrw rs1 csr)
   Write contents of rs1 to csr"
  (csrrw 'x0 rs1 csr ))

(defun csrs (rs1 csr)
  "(csrs rs1 csr)
   Sets the bits in the csr that are set in rs1"
  (csrrs 'x0 rs1 csr ))

(defun csrc (rs1 csr)
  "(csrc rs1 csr)
   Clears the bits in the csr that are set in rs1"
  (csrrc 'x0 rs1 csr ))

        ;;;;  derived immediate instructions  ;;;;

(defun csrwi (uimm5 csr)
 "(csrw uimm5 csr)
   Write zero extended immediate 'uimm5' to csr"
  (csrrwi 'x0 uimm5 csr ))

(defun csrsi (uimm5 csr)
  "(csrsi uimm5 csr)
   Sets the bits in the csr that are set in the immediate 'uimm5'"
  (csrrsi 'x0 uimm5 csr ))

(defun csrci (uimm5 csr)
  "(csrci uimm5 csr)
   Clears the bits in the csr that are set in the immediate 'uimm5'"
  (csrrci 'x0 uimm5 csr ))
