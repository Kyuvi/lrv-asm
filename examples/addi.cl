
(in-package cl-user)

(load "../lrv-korr/packages.lisp")
(load "../lrv-korr/env-lrv.cl")
(load "../lrv-korr/kone-lrv.cl")
(load "../lrv-korr/fmt-lrv.cl")
(load "../lrv-korr/files-lrv.cl")
(load "../lrv-korr/lrv-utils/clrv-utils.cl")

(load "../lrv-ins/I-lrv.cl")
(load "../lrv-ins/C-lrv.cl")
(load "../lrv-ins/I-C-lrv.cl")
(load "../lrv-ins/rvi-derived.cl")
(load "../lrv-ins/M-lrv.cl")
(load "../lrv-ins/csr-lrv.cl")


(defpackage "LONGAN"
  (:use :cl :rvasm :clrv :c32 :i32 :ic32 :rdv :m32 :csr :csr32)
  (:shadowing-import-from :ic32 and or beqz bnez ) ;; beqz & bnez also defined in rvdrv.
  (:shadowing-import-from :m32 rem )
  (:shadowing-import-from :rdv not ))


(in-package :longan)

;; set up chip specific environment

(defparameter *env* (make-instance 'basic-env :address 0))

(defparameter *max-address* 128000) ;; longan internal flash is 128kb

;; =code starts here=
(addi 'x10 'x0 #h20)  ;; load 20 into register x1
(addi 'x11 'x0 #y101)  ;; load 5 into register x1
(add 'x10 'x10 'x11)  ;; load 5 into register x1


;; set output file
(setf (bin-file "./binaries/addi.bin") (link *env*))
