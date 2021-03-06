
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
;; TODO change from rd rs1 csr to rd csr rs1

        ;;;; Register instructions ;;;;

(defun csrrw (rd csr rs1)
 "(csrrw rd rs1 csr)
  Reads contents of csr (zero extended) into rd and writes contents of rs1 to the csr.
  If rd = x0 does not read and only writes to csr."
  (emit-vait (csrreg csr rs1 1 rd)))

(defun csrrs (rd csr rs1)
 "(csrrs rd rs1 csr)
  Reads contents of csr (zero extended) into rd and sets the bits in the csr
  that are set in rs1. If rs1 = x0 does not write and only reads from csr."
  (emit-vait (csrreg csr rs1 2 rd)))

(defun csrrc (rd csr rs1)
 "(csrrc rd rs1 csr)
  Reads contents of csr (zero extended) into rd and clears the bits in the csr
  that are set in rs1. If rs1 = x0 does not write and only reads from csr."
  (emit-vait (csrreg csr rs1 3 rd)))

        ;;;; Immediate instructions ;;;;

(defun csrrwi (rd csr uimm5)
 "(csrrwi rd uimm5 csr)
  Reads contents of csr into rd and writes zero extended immediate 'uimm5' to the csr.
  If rd = x0 does not read and only writes to csr."
  (emit-vait (csrimm csr uimm5 5 rd)))

(defun csrrsi (rd csr uimm5)
 "(csrrsi rd uimm5 csr)
  Reads contents of csr (zero extended) into rd and sets the bits in the csr
  that are set in the immediate 'uimm5'.
  If rd = x0 does not read and only writes to csr."
  (emit-vait (csrimm csr uimm5 6 rd)))

(defun csrrci (rd csr uimm5)
 "(csrrci rd uimm5 csr)
  Reads contents of csr (zero extended) into rd and clears the bits in the csr
  that are set in the immediate 'uimm5'.
  If rd = x0 does not read and only writes to csr."
  (emit-vait (csrimm csr uimm5 7 rd)))

        ;;;; Derived register instructions ;;;;

(defun csrr (rd csr)
 "(csrr csr rd)
  Reads contents of csr (zero extended) to rd"
  (csrrs rd csr 'x0))

(defun csrw (csr rs)
 "(csrw csr rs1)
  Write contents of rs to csr"
  (csrrw 'x0 csr rs))

(defun csrs (csr rs)
 "(csrs csr rs1)
  Sets the bits in the csr that are set in rs"
  (csrrs 'x0 csr rs))

(defun csrc (csr rs)
 "(csrc csr rs1)
  Clears the bits in the csr that are set in rs"
  (csrrc 'x0 csr rs))

        ;;;;  Derived immediate instructions  ;;;;

(defun csrwi (csr uimm5)
 "(csrw csr uimm5)
  Write zero extended immediate 'uimm5' to csr"
  (csrrwi 'x0 csr uimm5))

(defun csrsi (csr uimm5)
 "(csrsi csr uimm5)
  Sets the bits in the csr that are set in the immediate 'uimm5'"
  (csrrsi 'x0 csr uimm5))

(defun csrci (csr uimm5)
 "(csrci csr uimm5)
  Clears the bits in the csr that are set in the immediate 'uimm5'"
  (csrrci 'x0 csr uimm5))


(defpackage "CSR-MMAP-32-RV"
  (:documentation "32-bit Risc-V control and status registers and related procedures
   constanst end in '??' and documentation is in the form  'csr description csr-access'
   csr-access is one of urw, uro, srw, sro, hrw, hro, mrw, mro
   u - user, s - supervisor, m - machine, r - read,  w - write, o - only")
  (:nicknames :csr32 :csrmm32)
  (:use :cl :rvasm :csr)
  (:export
           ;; User Trap Setup
           #:ustatus-csr?? #:uie-csr?? #:utvec-csr??
           ;; User Trap Handling
           #:uscratch-csr?? #:uepc-csr?? #:ucause-csr?? #:ubadaddr-csr?? #:utval-csr??
           #:uip-csr??
           ;; User Floating-Point CSRs
           #:fflags-csr?? #:frm-csr?? #:fcsr-csr??
           ;; User Timers and Counters
           #:cycle-csr?? #:time-csr?? #:instret-csr??
           #:hpmcounter3-csr?? #:hpmcounter4-csr?? #:hpmcounter5-csr?? #:hpmcounter6-csr??
           #:hpmcounter7-csr?? #:hpmcounter8-csr?? #:hpmcounter9-csr?? #:hpmcounter10-csr??
           #:hpmcounter11-csr?? #:hpmcounter12-csr?? #:hpmcounter13-csr?? #:hpmcounter14-csr??
           #:hpmcounter15-csr?? #:hpmcounter16-csr?? #:hpmcounter17-csr?? #:hpmcounter18-csr??
           #:hpmcounter19-csr?? #:hpmcounter20-csr?? #:hpmcounter21-csr?? #:hpmcounter22-csr??
           #:hpmcounter23-csr?? #:hpmcounter24-csr?? #:hpmcounter25-csr?? #:hpmcounter26-csr??
           #:hpmcounter27-csr?? #:hpmcounter28-csr?? #:hpmcounter29-csr?? #:hpmcounter30-csr??
           #:hpmcounter31-csr??
           #:cycleh-csr?? #:timeh-csr?? #:instreth-csr??
           #:hpmcounter3h-csr?? #:hpmcounter4h-csr?? #:hpmcounter5h-csr?? #:hpmcounter6h-csr??
           #:hpmcounter7h-csr?? #:hpmcounter8h-csr?? #:hpmcounter9h-csr??
           #:hpmcounter10h-csr?? #:hpmcounter11h-csr?? #:hpmcounter12h-csr??
           #:hpmcounter13h-csr?? #:hpmcounter14h-csr?? #:hpmcounter15h-csr??
           #:hpmcounter16h-csr?? #:hpmcounter17h-csr?? #:hpmcounter18h-csr??
           #:hpmcounter19h-csr?? #:hpmcounter20h-csr?? #:hpmcounter21h-csr??
           #:hpmcounter22h-csr?? #:hpmcounter23h-csr?? #:hpmcounter24h-csr??
           #:hpmcounter25h-csr?? #:hpmcounter26h-csr?? #:hpmcounter27h-csr??
           #:hpmcounter28h-csr?? #:hpmcounter29h-csr?? #:hpmcounter30h-csr??
           #:hpmcounter31h-csr??
           ;; Supervisor Trap Setup
           #:sstatus-csr?? #:sedeleg-csr?? #:sideleg-csr?? #:sie-csr??
           #:stvec-csr?? #:scounteren-csr??
           ;; Supervisor Trap Handling
           #:sscratch-csr?? #:sepc-csr?? #:scause-csr?? #:sbadaddr-csr?? #:stval-csr??
           #:sip-csr??
           ;; Supervisor Protection and Translation
           #:sptbr-csr?? #:satp-csr??
           ;; Machine Information Registers
           #:mvendorid-csr?? #:marchid-csr?? #:mimpid-csr?? #:mhartid-csr??
           ;; Machine Trap Setup
           #:mstatus-csr?? #:misa-csr?? #:medeleg-csr?? #:mideleg-csr?? #:mie-csr??
           #:mtvec-csr?? #:mcounteren-csr??
           ;; Machine Trap Handling
           #:mscratch-csr?? #:mepc-csr?? #:mcause-csr?? #:mbadaddr-csr?? #:mtval-csr??
           #:mip-csr??
           ;; Machine Protection and translation
           #:pmpcfg0-csr?? #:pmpcfg1-csr?? #:pmpcfg2-csr?? #:pmpcfg3-csr?? #:pmpaddr0-csr??
           #:pmpaddr1-csr?? #:pmpaddr2-csr?? #:pmpaddr3-csr?? #:pmpaddr4-csr??
           #:pmpaddr5-csr?? #:pmpaddr6-csr?? #:pmpaddr7-csr?? #:pmpaddr8-csr??
           #:pmpaddr9-csr?? #:pmpaddr10-csr?? #:pmpaddr11-csr?? #:pmpaddr12-csr??
           #:pmpaddr13-csr?? #:pmpaddr14-csr?? #:pmpaddr15-csr??
           ;; Machine Counters / Timers
           #:mcycle-csr?? #:mtime-csr?? #:minstret-csr??
           #:mhpmcounter3-csr?? #:mhpmcounter4-csr?? #:mhpmcounter5-csr??
           #:mhpmcounter6-csr?? #:mhpmcounter7-csr?? #:mhpmcounter8-csr?? #:mhpmcounter9-csr??
           #:mhpmcounter10-csr?? #:mhpmcounter11-csr?? #:mhpmcounter12-csr??
           #:mhpmcounter13-csr?? #:mhpmcounter14-csr?? #:mhpmcounter15-csr??
           #:mhpmcounter16-csr?? #:mhpmcounter17-csr?? #:mhpmcounter18-csr??
           #:mhpmcounter19-csr?? #:mhpmcounter20-csr?? #:mhpmcounter21-csr??
           #:mhpmcounter22-csr?? #:mhpmcounter23-csr?? #:mhpmcounter24-csr??
           #:mhpmcounter25-csr?? #:mhpmcounter26-csr?? #:mhpmcounter27-csr??
           #:mhpmcounter28-csr?? #:mhpmcounter29-csr?? #:mhpmcounter30-csr??
           #:mhpmcounter31-csr??
           #:mcycleh-csr?? #:mtimeh-csr?? #:minstreth-csr??
           #:mhpmcounter3h-csr?? #:mhpmcounter4h-csr?? #:mhpmcounter5h-csr??
           #:mhpmcounter6h-csr?? #:mhpmcounter7h-csr?? #:mhpmcounter8h-csr??
           #:mhpmcounter9h-csr?? #:mhpmcounter10h-csr?? #:mhpmcounter11h-csr??
           #:mhpmcounter12h-csr?? #:mhpmcounter13h-csr?? #:mhpmcounter14h-csr??
           #:mhpmcounter15h-csr?? #:mhpmcounter16h-csr?? #:mhpmcounter17h-csr??
           #:mhpmcounter18h-csr?? #:mhpmcounter19h-csr?? #:mhpmcounter20h-csr??
           #:mhpmcounter21h-csr?? #:mhpmcounter22h-csr?? #:mhpmcounter23h-csr??
           #:mhpmcounter24h-csr?? #:mhpmcounter25h-csr?? #:mhpmcounter26h-csr??
           #:mhpmcounter27h-csr?? #:mhpmcounter28h-csr?? #:mhpmcounter29h-csr??
           #:mhpmcounter30h-csr?? #:mhpmcounter31h-csr??
           ;; Machine Counter Setup
           #:mhpmevent3-csr?? #:mhpmevent4-csr?? #:mhpmevent5-csr?? #:mhpmevent6-csr??
           #:mhpmevent7-csr?? #:mhpmevent8-csr?? #:mhpmevent9-csr?? #:mhpmevent10-csr??
           #:mhpmevent11-csr?? #:mhpmevent12-csr?? #:mhpmevent13-csr?? #:mhpmevent14-csr??
           #:mhpmevent15-csr?? #:mhpmevent16-csr?? #:mhpmevent17-csr?? #:mhpmevent18-csr??
           #:mhpmevent19-csr?? #:mhpmevent20-csr?? #:mhpmevent21-csr?? #:mhpmevent22-csr??
           #:mhpmevent23-csr?? #:mhpmevent24-csr?? #:mhpmevent25-csr?? #:mhpmevent26-csr??
           #:mhpmevent27-csr?? #:mhpmevent28-csr?? #:mhpmevent29-csr?? #:mhpmevent30-csr??
           #:mhpmevent31-csr??
           ;; Debug/Trace Registers (shared with Debug Mode)
           #:tselect-csr?? #:tdata1-csr?? #:tdata2-csr?? #:tdata3-csr??
           ;; Debug Mode Registers
           #:dcsr-csr?? #:dpc-csr?? #:dscratch-csr??
           ;; Nonstandard
           ;; #:mtohost-csr?? #:mfromhost-csr?? #:mreset-csr?? #:mipi-csr??
           ;; #:miobase-csr?? #:csr-mtvt?? #:csr-mnxti?? #:csr-mtvt2?? #:csr-jalmnxti??
           ;; #:csr-pushmcause?? #:csr-pushmepc?? #:csr-pushmsubm?? #:csr-wfe?? #:csr-sleepvalue??
           ;; #:csr-txevt?? #:csr-mcountinhibit?? #:csr-mmisc-ctl?? #:csr-mnvec?? #:csr-msubm??

           ;; csr-utility functions
           #:rdtime #:rdtimeh #:read-time
   ))

(in-package :csrmm32)
;; format of a constant in this module:
;;(defconstant <csr name> <csr number>  "<csr description> <csr access>")
;;
;; <access> is one of urw, uro, srw, sro, hrw, hro, mrw, mro

;; User Trap Setup
(defconstant ustatus-csr?? #x000 "User status register (urw)")
(defconstant uie-csr?? #x004 "User interrupt-enable register (urw)")
(defconstant utvec-csr?? #x005 "User trap handler base address (urw)")

;; User Trap Handling
(defconstant uscratch-csr?? #x040 "Scratch handler for user trap handlers (urw)")
(defconstant uepc-csr?? #x041 "User exception program counter (urw)")
(defconstant ucause-csr?? #x042 "User trap cause (urw)")
(defconstant ubadaddr-csr?? #x043 "User bad address (urw)")
(defconstant utval-csr?? #x043 "User bad address or instruction (urw)")
(defconstant uip-csr?? #x044 "User interrupt pending (urw)")

;; User Floating-Point CSRs
(defconstant fflags-csr?? #x001 "Floating-Point Accrued Exceptions (urw)")
(defconstant frm-csr?? #x002 "Floating-Point Dynamic Rounding Mode (urw)")
(defconstant fcsr-csr?? #x003 "Floating-Point Control and Status (frm + fflags) (urw)")

;; User Timers and Counters
(defconstant cycle-csr?? #xC00 "Cycle counter (for RDCYCLE) (uro)")
(defconstant time-csr?? #xC01 "Wall-clock time (for RDTIME) (uro)")
(defconstant instret-csr?? #xC02 "Instructions-retired counter (for RDINSTRET) (uro)")
(defconstant hpmcounter3-csr?? #xC03 "Performance-monitoring counter (mrw)")
(defconstant hpmcounter4-csr?? #xC04 "Performance-monitoring counter (mrw)")
(defconstant hpmcounter5-csr?? #xC05 "Performance-monitoring counter (mrw)")
(defconstant hpmcounter6-csr?? #xC06 "Performance-monitoring counter (mrw)")
(defconstant hpmcounter7-csr?? #xC07 "Performance-monitoring counter (mrw)")
(defconstant hpmcounter8-csr?? #xC08 "Performance-monitoring counter (mrw)")
(defconstant hpmcounter9-csr?? #xC09 "Performance-monitoring counter (mrw)")
(defconstant hpmcounter10-csr?? #xC0A "Performance-monitoring counter (mrw)")
(defconstant hpmcounter11-csr?? #xC0B "Performance-monitoring counter (mrw)")
(defconstant hpmcounter12-csr?? #xC0C "Performance-monitoring counter (mrw)")
(defconstant hpmcounter13-csr?? #xC0D "Performance-monitoring counter (mrw)")
(defconstant hpmcounter14-csr?? #xC0E "Performance-monitoring counter (mrw)")
(defconstant hpmcounter15-csr?? #xC0F "Performance-monitoring counter (mrw)")
(defconstant hpmcounter16-csr?? #xC10 "Performance-monitoring counter (mrw)")
(defconstant hpmcounter17-csr?? #xC11 "Performance-monitoring counter (mrw)")
(defconstant hpmcounter18-csr?? #xC12 "Performance-monitoring counter (mrw)")
(defconstant hpmcounter19-csr?? #xC13 "Performance-monitoring counter (mrw)")
(defconstant hpmcounter20-csr?? #xC14 "Performance-monitoring counter (mrw)")
(defconstant hpmcounter21-csr?? #xC15 "Performance-monitoring counter (mrw)")
(defconstant hpmcounter22-csr?? #xC16 "Performance-monitoring counter (mrw)")
(defconstant hpmcounter23-csr?? #xC17 "Performance-monitoring counter (mrw)")
(defconstant hpmcounter24-csr?? #xC18 "Performance-monitoring counter (mrw)")
(defconstant hpmcounter25-csr?? #xC19 "Performance-monitoring counter (mrw)")
(defconstant hpmcounter26-csr?? #xC1A "Performance-monitoring counter (mrw)")
(defconstant hpmcounter27-csr?? #xC1B "Performance-monitoring counter (mrw)")
(defconstant hpmcounter28-csr?? #xC1C "Performance-monitoring counter (mrw)")
(defconstant hpmcounter29-csr?? #xC1D "Performance-monitoring counter (mrw)")
(defconstant hpmcounter30-csr?? #xC1E "Performance-monitoring counter (mrw)")
(defconstant hpmcounter31-csr?? #xC1F "Performance-monitoring counter (mrw)")


(defconstant cycleh-csr?? #xC80 "Upper 32 bits of cycle, RV32I only (uro)")
(defconstant timeh-csr?? #xC81 "Upper 32 bits of time, RV32I only (uro)")
(defconstant instreth-csr?? #xC82 "Upper 32 bits of instret, RV32I only (uro)")
(defconstant hpmcounter3h-csr?? #xC83 "Upper 32 bits of hpmcounter3, RV32I only (mrw)")
(defconstant hpmcounter4h-csr?? #xC84 "Upper 32 bits of hpmcounter4, RV32I only (mrw)")
(defconstant hpmcounter5h-csr?? #xC85 "Upper 32 bits of hpmcounter5, RV32I only (mrw)")
(defconstant hpmcounter6h-csr?? #xC86 "Upper 32 bits of hpmcounter6, RV32I only (mrw)")
(defconstant hpmcounter7h-csr?? #xC87 "Upper 32 bits of hpmcounter7, RV32I only (mrw)")
(defconstant hpmcounter8h-csr?? #xC88 "Upper 32 bits of hpmcounter8, RV32I only (mrw)")
(defconstant hpmcounter9h-csr?? #xC89 "Upper 32 bits of hpmcounter9, RV32I only (mrw)")
(defconstant hpmcounter10h-csr?? #xC8A "Upper 32 bits of hpmcounter10, RV32I only (mrw)")
(defconstant hpmcounter11h-csr?? #xC8B "Upper 32 bits of hpmcounter11, RV32I only (mrw)")
(defconstant hpmcounter12h-csr?? #xC8C "Upper 32 bits of hpmcounter12, RV32I only (mrw)")
(defconstant hpmcounter13h-csr?? #xC8D "Upper 32 bits of hpmcounter13, RV32I only (mrw)")
(defconstant hpmcounter14h-csr?? #xC8E "Upper 32 bits of hpmcounter14, RV32I only (mrw)")
(defconstant hpmcounter15h-csr?? #xC8F "Upper 32 bits of hpmcounter15, RV32I only (mrw)")
(defconstant hpmcounter16h-csr?? #xC90 "Upper 32 bits of hpmcounter16, RV32I only (mrw)")
(defconstant hpmcounter17h-csr?? #xC91 "Upper 32 bits of hpmcounter17, RV32I only (mrw)")
(defconstant hpmcounter18h-csr?? #xC92 "Upper 32 bits of hpmcounter18, RV32I only (mrw)")
(defconstant hpmcounter19h-csr?? #xC93 "Upper 32 bits of hpmcounter19, RV32I only (mrw)")
(defconstant hpmcounter20h-csr?? #xC94 "Upper 32 bits of hpmcounter20, RV32I only (mrw)")
(defconstant hpmcounter21h-csr?? #xC95 "Upper 32 bits of hpmcounter21, RV32I only (mrw)")
(defconstant hpmcounter22h-csr?? #xC96 "Upper 32 bits of hpmcounter22, RV32I only (mrw)")
(defconstant hpmcounter23h-csr?? #xC97 "Upper 32 bits of hpmcounter23, RV32I only (mrw)")
(defconstant hpmcounter24h-csr?? #xC98 "Upper 32 bits of hpmcounter24, RV32I only (mrw)")
(defconstant hpmcounter25h-csr?? #xC99 "Upper 32 bits of hpmcounter25, RV32I only (mrw)")
(defconstant hpmcounter26h-csr?? #xC9A "Upper 32 bits of hpmcounter26, RV32I only (mrw)")
(defconstant hpmcounter27h-csr?? #xC9B "Upper 32 bits of hpmcounter27, RV32I only (mrw)")
(defconstant hpmcounter28h-csr?? #xC9C "Upper 32 bits of hpmcounter28, RV32I only (mrw)")
(defconstant hpmcounter29h-csr?? #xC9D "Upper 32 bits of hpmcounter29, RV32I only (mrw)")
(defconstant hpmcounter30h-csr?? #xC9E "Upper 32 bits of hpmcounter30, RV32I only (mrw)")
(defconstant hpmcounter31h-csr?? #xC9F "Upper 32 bits of hpmcounter31, RV32I only (mrw)")

;; Supervisor Trap Setup
(defconstant sstatus-csr?? #x100 "Supervisor status register (srw)")
(defconstant sedeleg-csr?? #x102 "Supervisor exception delegation register (srw)")
(defconstant sideleg-csr?? #x103 "Supervisor interrupt delegation register (srw)")
(defconstant sie-csr?? #x104 "Supervisor interrupt-enable register (src)")
(defconstant stvec-csr?? #x105 "Supervisor trap handler base address (srw)")
(defconstant scounteren-csr?? #x106 "Supervisor counter enable (swr)")

;; Supervisor Trap Handling
(defconstant sscratch-csr?? #x140 "Scratch register for supervisor trap handlers (srw)")
(defconstant sepc-csr?? #x141 "Supervisor exception program counter (srw)")
(defconstant scause-csr?? #x142 "Supervisor trap cause (srw)")
(defconstant sbadaddr-csr?? #x143 "Supervisor bad address (srw)")
(defconstant stval-csr?? #x143 "Supervisor bad address or instruction (srw)")
(defconstant sip-csr?? #x144 "Supervisor interrupt pending (srw)")

;; Supervisor Protection and Translation
(defconstant sptbr-csr?? #x180 "Page-table base register (srw)")
(defconstant satp-csr?? #x180 "Supervisor address translation and protection (srw)")

;; ;; Supervisor Timers and Counters
;; ;; Could not find these in 2019 draft??
;; (defconstant scycle-csr?? #xD00 "Supervisor cycle counter (sro)")
;; (defconstant stime-csr?? #xD01 "Supervisor wall-clock time (sro)")
;; (defconstant sinstret-csr?? #xD02 "Supervisor instructions-retired counter (sro)")
;; (defconstant scycleh-csr?? #xD80 "Upper 32 bits of scycle, RV32I only (sro)")
;; (defconstant stimeh-csr?? #xD81 "Upper 32 bits of stime, RV32I only (sro)")
;; (defconstant sinstreth-csr?? #xD82 "Upper 32 bits of sinstret, RV32I only (sro)")

;; Hypervisor no longer in use
;; ;; Hypervisor Trap Setup
;; (defconstant hstatus-csr?? #x200 "Hypervisor status register (hrw)")
;; (defconstant hedeleg-csr?? #x202 "Hypervisor exception delegation register (mrw)")
;; (defconstant hideleg-csr?? #x203 "Hypervisor interrupt delegation register (mrw)")
;; (defconstant hie-csr?? #x204 "Hypervisor interrupt-enable register (mrw)")
;; (defconstant htvec-csr?? #x205 "Hypervisor trap handler base address (hrw)")

;; ;; Hypervisor Trap Handling
;; (defconstant hscratch-csr?? #x240 "Scratch register for hypervisor trap handlers (hrw)")
;; (defconstant hepc-csr?? #x241 "Hypervisor exception program counter (hrw)")
;; (defconstant hcause-csr?? #x242 "Hypervisor trap cause (hrw)")
;; (defconstant hbadaddr-csr?? #x243 "Hypervisor bad address (hrw)")
;; (defconstant hip-csr?? #x244 "Hypervisor interrupt pending (hrw)")

;; ;; Hypervisor Protection and Translation

;; ;; Hypervisor Timers and Counters
;; (defconstant hcycle-csr?? #xE00 "Hypervisor cycle counter (sro)")
;; (defconstant htime-csr?? #xE01 "Hypervisor wall-clock time (sro)")
;; (defconstant hinstret-csr?? #xE02 "Hypervisor instructions-retired counter (sro)")
;; (defconstant hcycleh-csr?? #xE80 "Upper 32 bits of hcycle, RV32I only (sro)")
;; (defconstant htimeh-csr?? #xE81 "Upper 32 bits of htime, RV32I only (sro)")
;; (defconstant hinstreth-csr?? #xE82 "Upper 32 bits of hinstret, RV32I only (sro)")

        ;;;; Machine-level CSRs ;;;;

;; Machine Information Registers
(defconstant mvendorid-csr?? #xF11 "Vendor ID (mro)")
(defconstant marchid-csr?? #xF12 "Architecture ID (mro)")
(defconstant mimpid-csr?? #xF13 "Implementation ID (mro)")
(defconstant mhartid-csr?? #xF14 "Hardware thread ID (mro)")

;; Machine Trap Setup
(defconstant mstatus-csr?? #x300 "Machine status register (mrw)")
(defconstant misa-csr?? #x301 "ISA and extensions supported (mrw)")
(defconstant medeleg-csr?? #x302 "Machine exception delegation register (mrw)")
(defconstant mideleg-csr?? #x303 "Machine interrupt delegation register (mrw)")
(defconstant mie-csr?? #x304 "Machine interrupt-enable register (mrw)")
(defconstant mtvec-csr?? #x305 "Machine trap-vector base address (mrw)")
;; Only exists if there is user mode bit 0 - CY, bit 1 - TM, bit 2 - IR
(defconstant mcounteren-csr?? #x306 "Machine counter enable (mrw)")

;; Machine Trap Handling
(defconstant mscratch-csr?? #x340 "Scratch register for machine trap handlers (mrw)")
(defconstant mepc-csr?? #x341
  "Machine exception program counter (mrw)
  Holds the address of the instruction that was interrupted or encountered
  the exception (or written by software)")
(defconstant mcause-csr?? #x342
  "Machine trap cause (mrw)
  Holds the code indicating the event that caused the trap, with bit mxlen-1
  set if the trap was caused by an interrupt (or written by software)")
(defconstant mbadaddr-csr?? #x343 "Machine bad address (mrw)")
(defconstant mtval-csr?? #x343 "Machine bad address or instruction (mrw)")
(defconstant mip-csr?? #x344 "Machine interrupt pending (mrw)")

;; Machine Protection and translation
(defconstant pmpcfg0-csr?? #x3A0 "Physical memory protection configuration (mrw)")
(defconstant pmpcfg1-csr?? #x3A1 "Physical memory protection configuration (RV32 only) (mrw)")
(defconstant pmpcfg2-csr?? #x3A2 "Physical memory protection configuration (mrw)")
(defconstant pmpcfg3-csr?? #x3A3 "Physical memory protection configuration (RV32 only) (mrw)")
(defconstant pmpaddr0-csr?? #x3B0 "Physical memory protection address register (mrw)")
(defconstant pmpaddr1-csr?? #x3B1 "Physical memory protection address register (mrw)")
(defconstant pmpaddr2-csr?? #x3B2 "Physical memory protection address register (mrw)")
(defconstant pmpaddr3-csr?? #x3B3 "Physical memory protection address register (mrw)")
(defconstant pmpaddr4-csr?? #x3B4 "Physical memory protection address register (mrw)")
(defconstant pmpaddr5-csr?? #x3B5 "Physical memory protection address register (mrw)")
(defconstant pmpaddr6-csr?? #x3B6 "Physical memory protection address register (mrw)")
(defconstant pmpaddr7-csr?? #x3B7 "Physical memory protection address register (mrw)")
(defconstant pmpaddr8-csr?? #x3B8 "Physical memory protection address register (mrw)")
(defconstant pmpaddr9-csr?? #x3B9 "Physical memory protection address register (mrw)")
(defconstant pmpaddr10-csr?? #x3BA "Physical memory protection address register (mrw)")
(defconstant pmpaddr11-csr?? #x3BB "Physical memory protection address register (mrw)")
(defconstant pmpaddr12-csr?? #x3BC "Physical memory protection address register (mrw)")
(defconstant pmpaddr13-csr?? #x3BE "Physical memory protection address register (mrw)")
(defconstant pmpaddr14-csr?? #x3BD "Physical memory protection address register (mrw)")
(defconstant pmpaddr15-csr?? #x3BF "Physical memory protection address register (mrw)")

;; ;; Machine Protection and Translation
;; ;; Could not find these in 2019 draft??
;; (defconstant mbase-csr?? #x380 "Base register (mrw)")
;; (defconstant mbound-csr?? #x381 "Bound register (mrw)")
;; (defconstant mibase-csr?? #x382 "Instruction base register (mrw)")
;; (defconstant mibound-csr?? #x383 "Instruction bound register (mrw)")
;; (defconstant mdbase-csr?? #x384 "Data base register (mrw)")
;; (defconstant mdbound-csr?? #x385 "Data bound register (mrw)")

;; Machine Counters / Timers
(defconstant mcycle-csr?? #xB00 "Machine cycle counter (mrw)")
(defconstant mtime-csr?? #xB01 "Machine wall-clock time (mrw)")
(defconstant minstret-csr?? #xB02 "Machine instructions-retired counter (mrw)")
(defconstant mhpmcounter3-csr?? #xB03 "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter4-csr?? #xB04 "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter5-csr?? #xB05 "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter6-csr?? #xB06 "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter7-csr?? #xB07 "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter8-csr?? #xB08 "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter9-csr?? #xB09 "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter10-csr?? #xB0A "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter11-csr?? #xB0B "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter12-csr?? #xB0C "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter13-csr?? #xB0D "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter14-csr?? #xB0E "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter15-csr?? #xB0F "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter16-csr?? #xB10 "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter17-csr?? #xB11 "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter18-csr?? #xB12 "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter19-csr?? #xB13 "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter20-csr?? #xB14 "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter21-csr?? #xB15 "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter22-csr?? #xB16 "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter23-csr?? #xB17 "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter24-csr?? #xB18 "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter25-csr?? #xB19 "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter26-csr?? #xB1A "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter27-csr?? #xB1B "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter28-csr?? #xB1C "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter29-csr?? #xB1D "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter30-csr?? #xB1E "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter31-csr?? #xB1F "Machine performance-monitoring counter (mrw)")


(defconstant mcycleh-csr?? #xB80 "Upper 32 bits of mcycle, RV32I only (mrw)")
(defconstant mtimeh-csr?? #xB81 "Upper 32 bits of mtime, RV32I only (mrw)")
(defconstant minstreth-csr?? #xB82 "Upper 32 bits of minstret, RV32I only (mrw)")
(defconstant mhpmcounter3h-csr?? #xB83 "Upper 32 bits of mhpmcounter3, RV32I only (mrw)")
(defconstant mhpmcounter4h-csr?? #xB84 "Upper 32 bits of mhpmcounter4, RV32I only (mrw)")
(defconstant mhpmcounter5h-csr?? #xB85 "Upper 32 bits of mhpmcounter5, RV32I only (mrw)")
(defconstant mhpmcounter6h-csr?? #xB86 "Upper 32 bits of mhpmcounter6, RV32I only (mrw)")
(defconstant mhpmcounter7h-csr?? #xB87 "Upper 32 bits of mhpmcounter7, RV32I only (mrw)")
(defconstant mhpmcounter8h-csr?? #xB88 "Upper 32 bits of mhpmcounter8, RV32I only (mrw)")
(defconstant mhpmcounter9h-csr?? #xB89 "Upper 32 bits of mhpmcounter9, RV32I only (mrw)")
(defconstant mhpmcounter10h-csr?? #xB8A "Upper 32 bits of mhpmcounter10, RV32I only (mrw)")
(defconstant mhpmcounter11h-csr?? #xB8B "Upper 32 bits of mhpmcounter11, RV32I only (mrw)")
(defconstant mhpmcounter12h-csr?? #xB8C "Upper 32 bits of mhpmcounter12, RV32I only (mrw)")
(defconstant mhpmcounter13h-csr?? #xB8D "Upper 32 bits of mhpmcounter13, RV32I only (mrw)")
(defconstant mhpmcounter14h-csr?? #xB8E "Upper 32 bits of mhpmcounter14, RV32I only (mrw)")
(defconstant mhpmcounter15h-csr?? #xB8F "Upper 32 bits of mhpmcounter15, RV32I only (mrw)")
(defconstant mhpmcounter16h-csr?? #xB90 "Upper 32 bits of mhpmcounter16, RV32I only (mrw)")
(defconstant mhpmcounter17h-csr?? #xB91 "Upper 32 bits of mhpmcounter17, RV32I only (mrw)")
(defconstant mhpmcounter18h-csr?? #xB92 "Upper 32 bits of mhpmcounter18, RV32I only (mrw)")
(defconstant mhpmcounter19h-csr?? #xB93 "Upper 32 bits of mhpmcounter19, RV32I only (mrw)")
(defconstant mhpmcounter20h-csr?? #xB94 "Upper 32 bits of mhpmcounter20, RV32I only (mrw)")
(defconstant mhpmcounter21h-csr?? #xB95 "Upper 32 bits of mhpmcounter21, RV32I only (mrw)")
(defconstant mhpmcounter22h-csr?? #xB96 "Upper 32 bits of mhpmcounter22, RV32I only (mrw)")
(defconstant mhpmcounter23h-csr?? #xB97 "Upper 32 bits of mhpmcounter23, RV32I only (mrw)")
(defconstant mhpmcounter24h-csr?? #xB98 "Upper 32 bits of mhpmcounter24, RV32I only (mrw)")
(defconstant mhpmcounter25h-csr?? #xB99 "Upper 32 bits of mhpmcounter25, RV32I only (mrw)")
(defconstant mhpmcounter26h-csr?? #xB9A "Upper 32 bits of mhpmcounter26, RV32I only (mrw)")
(defconstant mhpmcounter27h-csr?? #xB9B "Upper 32 bits of mhpmcounter27, RV32I only (mrw)")
(defconstant mhpmcounter28h-csr?? #xB9C "Upper 32 bits of mhpmcounter28, RV32I only (mrw)")
(defconstant mhpmcounter29h-csr?? #xB9D "Upper 32 bits of mhpmcounter29, RV32I only (mrw)")
(defconstant mhpmcounter30h-csr?? #xB9E "Upper 32 bits of mhpmcounter30, RV32I only (mrw)")
(defconstant mhpmcounter31h-csr?? #xB9F "Upper 32 bits of mhpmcounter31, RV32I only (mrw)")

;; Machine Counter Setup
;; These three taken out 1.9-1
;; (defconstant mucounteren-csr?? #x320 "Machine interrupt-enable register (mrw)")
;; (defconstant mscounteren-csr?? #x321 "Supervisor-mode counter enable (mrw)")
;; (defconstant mhcounteren-csr?? #x322 "Hypervisor-mode counter enable (mrw)")
(defconstant mhpmevent3-csr?? #x323 "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent4-csr?? #x324 "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent5-csr?? #x325 "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent6-csr?? #x326 "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent7-csr?? #x327 "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent8-csr?? #x328 "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent9-csr?? #x329 "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent10-csr?? #x32A "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent11-csr?? #x32B "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent12-csr?? #x32C "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent13-csr?? #x32D "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent14-csr?? #x32E "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent15-csr?? #x32F "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent16-csr?? #x330 "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent17-csr?? #x331 "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent18-csr?? #x332 "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent19-csr?? #x333 "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent20-csr?? #x334 "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent21-csr?? #x335 "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent22-csr?? #x336 "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent23-csr?? #x337 "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent24-csr?? #x338 "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent25-csr?? #x339 "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent26-csr?? #x33A "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent27-csr?? #x33B "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent28-csr?? #x33C "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent29-csr?? #x33D "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent30-csr?? #x33E "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent31-csr?? #x33F "Machine performance-monitoring event selector (mrw)")

;; Debug/Trace Registers (shared with Debug Mode)
(defconstant tselect-csr?? #x7A0 "Debug/Trace trigger register select (mrw)")
(defconstant tdata1-csr?? #x7A1 "First Debug/Trace trigger data register (mrw)")
(defconstant tdata2-csr?? #x7A2 "Second Debug/Trace trigger data register (mrw)")
(defconstant tdata3-csr?? #x7A3 "Third Debug/Trace trigger data register (mrw)")

;; Debug Mode Registers
(defconstant dcsr-csr?? #x7B0 "Debug control and status register (mrw)")
(defconstant dpc-csr?? #x7B1 "Debug program counter (mrw)")
(defconstant dscratch-csr?? #x7B2 "Debug scratch register (mrw)")

;; Nonstandard
;; (defconstant mtohost-csr?? #x780 "IO to Host (mrw)")
;; (defconstant mfromhost-csr?? #x781 "IO from Host (mrw)")
;; (defconstant mreset-csr?? #x782 "Reset (mrw)")
;; (defconstant mipi-csr?? #x783 "Inter Processor Interrupt (mrw)")
;; (defconstant miobase-csr?? #x784 "IO Base (mrw)")

;; Not in standard?
;; (defconstant csr-mtvt?? #x307)
;; (defconstant csr-mnxti?? #x345)


;; (defconstant csr-mtvt2?? #x7ec)
;; (defconstant csr-jalmnxti?? #x7ed)
;; (defconstant csr-pushmcause?? #x7ee)
;; (defconstant csr-pushmepc?? #x7ef)
;; (defconstant csr-pushmsubm?? #x7eb)

;; (defconstant csr-wfe?? #x810) ;;
;; (defconstant csr-sleepvalue?? #x811) ;;
;; (defconstant csr-txevt?? #x812) ;;

;; (defconstant csr-mcountinhibit?? #x320 "Used to stop mcount. Bit0 - cycles, Bit2 - instret")
;; (defconstant csr-mmisc-ctl?? #x7d0) ;;
;; (defconstant csr-mnvec?? #x7c3)
;; (defconstant csr-msubm?? #x7c4) ;;


        ;;;; csr-utility functions ;;;;

(defun rdtime (reg)
  (csrr reg time-csr??))

(defun rdtimeh (reg)
  (csrr reg timeh-csr??))

(defun read-time (reg1 reg2 &optional (reg3 t1))
 "Read the 64-bit time from the time csr 'csr-time'
  leaving the results in reg1 and reg2"
  (let ((read-time-label (gensym "READ-TIME-")))
    (set-label read-time-label)
    (rdtimeh reg1)
    (rdtime reg2)
    (rdtimeh reg3)
    (bne reg1 reg3 read-time-label)
  ))
