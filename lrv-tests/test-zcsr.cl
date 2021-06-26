
(load "../lrv-korr/packages.lisp")
(load "../lrv-korr/env-lrv.cl")
(load "../lrv-korr/fmt-lrv.cl")
(load "../lrv-korr/kone-lrv.cl")

(load "./test-rvasm.cl")

;; (load "~/.eclrc")
(load "../lrv-ins/csr-lrv.cl")

(defpackage "TEST-ZICSR"
  (:documentation "Test functions for the risc-v 32 bit compressed instructions")
  (:use :cl :rvasm :zicsr-rv :test-rvasm)
  )

(in-package :test-zicsr)

(do-vait-test csrrw
  ((x0 x0 #x0      #b00000000000000000001000001110011)
   (x0 x0 #xFFF    #b11111111111100000001000001110011)
   (x0 x31 #x0     #b00000000000011111001000001110011)
   (x31 x0 #x0     #b00000000000000000001111111110011)
   (x0 x31 #xFFF   #b11111111111111111001000001110011)
   (x31 x0 #xFFF   #b11111111111100000001111111110011)
   (x31 x31 #x0    #b00000000000011111001111111110011)
   (x31 x31 #xFFF  #b11111111111111111001111111110011))
  )

(do-vait-test csrrs
  ((x0 x0 #x0      #b00000000000000000010000001110011)
   (x0 x0 #xFFF    #b11111111111100000010000001110011)
   (x0 x31 #x0     #b00000000000011111010000001110011)
   (x31 x0 #x0     #b00000000000000000010111111110011)
   (x0 x31 #xFFF   #b11111111111111111010000001110011)
   (x31 x0 #xFFF   #b11111111111100000010111111110011)
   (x31 x31 #x0    #b00000000000011111010111111110011)
   (x31 x31 #xFFF  #b11111111111111111010111111110011))
  )

(do-vait-test csrrc
  ((x0 x0 #x0      #b00000000000000000011000001110011)
   (x0 x0 #xFFF    #b11111111111100000011000001110011)
   (x0 x31 #x0     #b00000000000011111011000001110011)
   (x31 x0 #x0     #b00000000000000000011111111110011)
   (x0 x31 #xFFF   #b11111111111111111011000001110011)
   (x31 x0 #xFFF   #b11111111111100000011111111110011)
   (x31 x31 #x0    #b00000000000011111011111111110011)
   (x31 x31 #xFFF  #b11111111111111111011111111110011))
  )

(do-vait-test csrrwi
  ((x0 0 #x0       #b00000000000000000101000001110011)
   (x0 0 #xFFF     #b11111111111100000101000001110011)
   (x0 31 #x0      #b00000000000011111101000001110011)
   (x31 0 #x0      #b00000000000000000101111111110011)
   (x0 31 #xFFF    #b11111111111111111101000001110011)
   (x31 0 #xFFF    #b11111111111100000101111111110011)
   (x31 31 #x0     #b00000000000011111101111111110011)
   (x31 31 #xFFF   #b11111111111111111101111111110011))
  )

(do-vait-test csrrsi
  ((x0 0 #x0       #b00000000000000000110000001110011)
   (x0 0 #xFFF     #b11111111111100000110000001110011)
   (x0 31 #x0      #b00000000000011111110000001110011)
   (x31 0 #x0      #b00000000000000000110111111110011)
   (x0 31 #xFFF    #b11111111111111111110000001110011)
   (x31 0 #xFFF    #b11111111111100000110111111110011)
   (x31 31 #x0     #b00000000000011111110111111110011)
   (x31 31 #xFFF   #b11111111111111111110111111110011))
  )

(do-vait-test csrrci
  ((x0 0 #x0       #b00000000000000000111000001110011)
   (x0 0 #xFFF     #b11111111111100000111000001110011)
   (x0 31 #x0      #b00000000000011111111000001110011)
   (x31 0 #x0      #b00000000000000000111111111110011)
   (x0 31 #xFFF    #b11111111111111111111000001110011)
   (x31 0 #xFFF    #b11111111111100000111111111110011)
   (x31 31 #x0     #b00000000000011111111111111110011)
   (x31 31 #xFFF   #b11111111111111111111111111110011))
  )
