
(defpackage "RVASM"
  ;; (:use :cl :rvreg)
  ;; (:shadow zero ra sp gp tp fp s0 s1 )
  ;; (:shadow or and)
  (:export 
;; files-lrv.cl
            #:write-binary-file #:bin-file #:read-binary-file

;; kone-lrv.cl
            ;; #:zero #:ra #:sp #:gp #:tp #:s0 #:fp #:s1 ;;  #:x16 #:x32
            #:*pc* #:regno #:cregp #:cregno #:rv-error
            #:build-expr-code #:build-expr-code* #:immp #:uimmp #:bits #:offset
            ;; #:lnot-imm #:lnotb #:lnotj #:lnotv #:lnotz

;; env-lrv.cl
            #:code-vector #:symbol-table
            #:*lazy-marker* #:promise #:promise-p #:*memoize-promises*
            #:force #:set-promise-value #:parse-binding #:forcing #:delay
            #:pure-delay
            #:resolve-tree
            #:get-byte
            #:encode-byte #:encode-jait #:encode-vait #:encode-zait #:encode-yait
            #:join-masks

            ;; (defgeneric (setf env-address) (address env)
            #:env-emit #:env-address #:env-code-vector #:env-symbol-table
            #:env-find-label #:env-set-label
            #:env-emit-ins
            #:link #:resolve-vector
            #:delegate-env #:delegate-code-vector #:delegate-symbol-lookup
            #:delegate-symbol-definition #:local-symbol-table
            #:basic-env #:local-env
            #:*env* #:*origin* #:*max-address*
            #:emit #:emit-byte #:emit-jait #:emit-vait #:emit-zait
            #:advance-to #:align
            #:label #:set-label #:label-difference #:label-inc
            ;; #:with-label #:usoro

;; fmt-lrv.cl
            #:creg #:cimm #:ciwid #:cismal #:cload #:cstore #:cstst #:cjump
            #:cbranch #:carith #:register #:immed #:branch #:jump #:store
            #:upperimm #:muldiv #:csrimm #:csrreg
            ;; #:define-immediate-type
            ))


(defpackage "RVASM-UTILS"
  (:documentation "Procedures that expand on the basic rvasm package")
  (:nicknames :rvutl)
  (:use :cl :clrv :rvreg :rvasm)
  ;; (:import-from :clrv symbol-append)
  ;; (:shadowing-import-from :clrv symbol-append char-code-list)
  (:export
          #:set-addressed-label #:weak-label
          #:set-rvdoc #:rvdoc
          #:var-rv #:multiple-var-rv #:svec-rv #:str-rv #:ascis #:asciz
          #:rv-srec #:make-rv-srec #:rv-srec-p #:rv-srec-vars #:def-srec-rv
          #:srec-rv #:emit-srec-rv
          #:with-label #:usoro
          #:lnot-imm #:lnotb #:lnotj #:lnotv #:lnotz
          #:bitsv #:bitsj #:bitsb
          #:jlsb #:jmsb
   ))


;; (in-package "RVASM")

;; (load "~/.eclrc")
