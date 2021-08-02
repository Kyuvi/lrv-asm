
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
   Reads contents of csr (zero extended) into rd and writes contents of rs1 to the csr.
   If rd = x0 does not read and only writes to csr."
  (emit-vait (csrreg csr rs1 1 rd)))

(defun csrrs (rd rs1 csr)
  "(csrrs rd rs1 csr)
   Reads contents of csr (zero extended) into rd and sets the bits in the csr
   that are set in rs1. If rs1 = x0 does not write and only reads from csr."
  (emit-vait (csrreg csr rs1 2 rd)))

(defun csrrc (rd rs1 csr)
  "(csrrc rd rs1 csr)
   Reads contents of csr (zero extended) into rd and clears the bits in the csr
   that are set in rs1. If rs1 = x0 does not write and only reads from csr."
  (emit-vait (csrreg csr rs1 3 rd)))

        ;;;; immediate instructions ;;;;

(defun csrrwi (rd uimm5 csr)
  "(csrrwi rd uimm5 csr)
   Reads contents of csr into rd and writes zero extended immediate 'uimm5' to the csr.
   If rd = x0 does not read and only writes to csr."
  (emit-vait (csrimm csr uimm5 5 rd)))

(defun csrrsi (rd uimm5 csr)
  "(csrrsi rd uimm5 csr)
   Reads contents of csr (zero extended) into rd and sets the bits in the csr
   that are set in the immediate 'uimm5'.
   If rd = x0 does not read and only writes to csr."
  (emit-vait (csrimm csr uimm5 6 rd)))

(defun csrrci (rd uimm5 csr)
  "(csrrci rd uimm5 csr)
   Reads contents of csr (zero extended) into rd and clears the bits in the csr
   that are set in the immediate 'uimm5'.
   If rd = x0 does not read and only writes to csr."
  (emit-vait (csrimm csr uimm5 7 rd)))

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


(defpackage "CSR-MMAP-32-RV"
  (:documentation "32-bit Risc-V control and status registers and related procedures
   constanst end in 'Φ' and documentation is in the form  'csr description csr-access'
   csr-access is one of urw, uro, srw, sro, hrw, hro, mrw, mro
   u - user, s - supervisor, m - machine, r - read,  w - write, o - only")
  (:nicknames :csr32 :csrmm32)
  (:use :cl :rvasm :csr)
  (:export
           ;; User Trap Setup
           #:ustatus-csrΦ #:uie-csrΦ #:utvec-csrΦ
           ;; User Trap Handling
           #:uscratch-csrΦ #:uepc-csrΦ #:ucause-csrΦ #:ubadaddr-csrΦ #:utval-csrΦ
           #:uip-csrΦ
           ;; User Floating-Point CSRs
           #:fflags-csrΦ #:frm-csrΦ #:fcsr-csrΦ
           ;; User Timers and Counters
           #:cycle-csrΦ #:time-csrΦ #:instret-csrΦ
           #:hpmcounter3-csrΦ #:hpmcounter4-csrΦ #:hpmcounter5-csrΦ #:hpmcounter6-csrΦ
           #:hpmcounter7-csrΦ #:hpmcounter8-csrΦ #:hpmcounter9-csrΦ #:hpmcounter10-csrΦ
           #:hpmcounter11-csrΦ #:hpmcounter12-csrΦ #:hpmcounter13-csrΦ #:hpmcounter14-csrΦ
           #:hpmcounter15-csrΦ #:hpmcounter16-csrΦ #:hpmcounter17-csrΦ #:hpmcounter18-csrΦ
           #:hpmcounter19-csrΦ #:hpmcounter20-csrΦ #:hpmcounter21-csrΦ #:hpmcounter22-csrΦ
           #:hpmcounter23-csrΦ #:hpmcounter24-csrΦ #:hpmcounter25-csrΦ #:hpmcounter26-csrΦ
           #:hpmcounter27-csrΦ #:hpmcounter28-csrΦ #:hpmcounter29-csrΦ #:hpmcounter30-csrΦ
           #:hpmcounter31-csrΦ
           #:cycleh-csrΦ #:timeh-csrΦ #:instreth-csrΦ
           #:hpmcounter3h-csrΦ #:hpmcounter4h-csrΦ #:hpmcounter5h-csrΦ #:hpmcounter6h-csrΦ
           #:hpmcounter7h-csrΦ #:hpmcounter8h-csrΦ #:hpmcounter9h-csrΦ
           #:hpmcounter10h-csrΦ #:hpmcounter11h-csrΦ #:hpmcounter12h-csrΦ
           #:hpmcounter13h-csrΦ #:hpmcounter14h-csrΦ #:hpmcounter15h-csrΦ
           #:hpmcounter16h-csrΦ #:hpmcounter17h-csrΦ #:hpmcounter18h-csrΦ
           #:hpmcounter19h-csrΦ #:hpmcounter20h-csrΦ #:hpmcounter21h-csrΦ
           #:hpmcounter22h-csrΦ #:hpmcounter23h-csrΦ #:hpmcounter24h-csrΦ
           #:hpmcounter25h-csrΦ #:hpmcounter26h-csrΦ #:hpmcounter27h-csrΦ
           #:hpmcounter28h-csrΦ #:hpmcounter29h-csrΦ #:hpmcounter30h-csrΦ
           #:hpmcounter31h-csrΦ
           ;; Supervisor Trap Setup
           #:sstatus-csrΦ #:sedeleg-csrΦ #:sideleg-csrΦ #:sie-csrΦ
           #:stvec-csrΦ #:scounteren-csrΦ
           ;; Supervisor Trap Handling
           #:sscratch-csrΦ #:sepc-csrΦ #:scause-csrΦ #:sbadaddr-csrΦ #:stval-csrΦ
           #:sip-csrΦ
           ;; Supervisor Protection and Translation
           #:sptbr-csrΦ #:satp-csrΦ
           ;; Machine Information Registers
           #:mvendorid-csrΦ #:marchid-csrΦ #:mimpid-csrΦ #:mhartid-csrΦ
           ;; Machine Trap Setup
           #:mstatus-csrΦ #:misa-csrΦ #:medeleg-csrΦ #:mideleg-csrΦ #:mie-csrΦ
           #:mtvec-csrΦ #:mcounteren-csrΦ
           ;; Machine Trap Handling
           #:mscratch-csrΦ #:mepc-csrΦ #:mcause-csrΦ #:mbadaddr-csrΦ #:mtval-csrΦ
           #:mip-csrΦ
           ;; Machine Protection and translation
           #:pmpcfg0-csrΦ #:pmpcfg1-csrΦ #:pmpcfg2-csrΦ #:pmpcfg3-csrΦ #:pmpaddr0-csrΦ
           #:pmpaddr1-csrΦ #:pmpaddr2-csrΦ #:pmpaddr3-csrΦ #:pmpaddr4-csrΦ
           #:pmpaddr5-csrΦ #:pmpaddr6-csrΦ #:pmpaddr7-csrΦ #:pmpaddr8-csrΦ
           #:pmpaddr9-csrΦ #:pmpaddr10-csrΦ #:pmpaddr11-csrΦ #:pmpaddr12-csrΦ
           #:pmpaddr13-csrΦ #:pmpaddr14-csrΦ #:pmpaddr15-csrΦ
           ;; Machine Counters / Timers
           #:mcycle-csrΦ #:mtime-csrΦ #:minstret-csrΦ
           #:mhpmcounter3-csrΦ #:mhpmcounter4-csrΦ #:mhpmcounter5-csrΦ
           #:mhpmcounter6-csrΦ #:mhpmcounter7-csrΦ #:mhpmcounter8-csrΦ #:mhpmcounter9-csrΦ
           #:mhpmcounter10-csrΦ #:mhpmcounter11-csrΦ #:mhpmcounter12-csrΦ
           #:mhpmcounter13-csrΦ #:mhpmcounter14-csrΦ #:mhpmcounter15-csrΦ
           #:mhpmcounter16-csrΦ #:mhpmcounter17-csrΦ #:mhpmcounter18-csrΦ
           #:mhpmcounter19-csrΦ #:mhpmcounter20-csrΦ #:mhpmcounter21-csrΦ
           #:mhpmcounter22-csrΦ #:mhpmcounter23-csrΦ #:mhpmcounter24-csrΦ
           #:mhpmcounter25-csrΦ #:mhpmcounter26-csrΦ #:mhpmcounter27-csrΦ
           #:mhpmcounter28-csrΦ #:mhpmcounter29-csrΦ #:mhpmcounter30-csrΦ
           #:mhpmcounter31-csrΦ
           #:mcycleh-csrΦ #:mtimeh-csrΦ #:minstreth-csrΦ
           #:mhpmcounter3h-csrΦ #:mhpmcounter4h-csrΦ #:mhpmcounter5h-csrΦ
           #:mhpmcounter6h-csrΦ #:mhpmcounter7h-csrΦ #:mhpmcounter8h-csrΦ
           #:mhpmcounter9h-csrΦ #:mhpmcounter10h-csrΦ #:mhpmcounter11h-csrΦ
           #:mhpmcounter12h-csrΦ #:mhpmcounter13h-csrΦ #:mhpmcounter14h-csrΦ
           #:mhpmcounter15h-csrΦ #:mhpmcounter16h-csrΦ #:mhpmcounter17h-csrΦ
           #:mhpmcounter18h-csrΦ #:mhpmcounter19h-csrΦ #:mhpmcounter20h-csrΦ
           #:mhpmcounter21h-csrΦ #:mhpmcounter22h-csrΦ #:mhpmcounter23h-csrΦ
           #:mhpmcounter24h-csrΦ #:mhpmcounter25h-csrΦ #:mhpmcounter26h-csrΦ
           #:mhpmcounter27h-csrΦ #:mhpmcounter28h-csrΦ #:mhpmcounter29h-csrΦ
           #:mhpmcounter30h-csrΦ #:mhpmcounter31h-csrΦ
           ;; Machine Counter Setup
           #:mhpmevent3-csrΦ #:mhpmevent4-csrΦ #:mhpmevent5-csrΦ #:mhpmevent6-csrΦ
           #:mhpmevent7-csrΦ #:mhpmevent8-csrΦ #:mhpmevent9-csrΦ #:mhpmevent10-csrΦ
           #:mhpmevent11-csrΦ #:mhpmevent12-csrΦ #:mhpmevent13-csrΦ #:mhpmevent14-csrΦ
           #:mhpmevent15-csrΦ #:mhpmevent16-csrΦ #:mhpmevent17-csrΦ #:mhpmevent18-csrΦ
           #:mhpmevent19-csrΦ #:mhpmevent20-csrΦ #:mhpmevent21-csrΦ #:mhpmevent22-csrΦ
           #:mhpmevent23-csrΦ #:mhpmevent24-csrΦ #:mhpmevent25-csrΦ #:mhpmevent26-csrΦ
           #:mhpmevent27-csrΦ #:mhpmevent28-csrΦ #:mhpmevent29-csrΦ #:mhpmevent30-csrΦ
           #:mhpmevent31-csrΦ
           ;; Debug/Trace Registers (shared with Debug Mode)
           #:tselect-csrΦ #:tdata1-csrΦ #:tdata2-csrΦ #:tdata3-csrΦ
           ;; Debug Mode Registers
           #:dcsr-csrΦ #:dpc-csrΦ #:dscratch-csrΦ
           ;; Nonstandard
           ;; #:mtohost-csrΦ #:mfromhost-csrΦ #:mreset-csrΦ #:mipi-csrΦ
           ;; #:miobase-csrΦ #:csr-mtvtΦ #:csr-mnxtiΦ #:csr-mtvt2Φ #:csr-jalmnxtiΦ
           ;; #:csr-pushmcauseΦ #:csr-pushmepcΦ #:csr-pushmsubmΦ #:csr-wfeΦ #:csr-sleepvalueΦ
           ;; #:csr-txevtΦ #:csr-mcountinhibitΦ #:csr-mmisc-ctlΦ #:csr-mnvecΦ #:csr-msubmΦ

           ;; csr-utility functions
           #:rdtime #:rdtimeh #:read-time
   ))

(in-package :csrmm32)
;; format of a constant in this module:
;;(defconstant <csr name> <csr number>   "<csr description> <csr access>")
;;
;; <access> is one of urw, uro, srw, sro, hrw, hro, mrw, mro

;; User Trap Setup
(defconstant ustatus-csrΦ #x000 "User status register (urw)")
(defconstant uie-csrΦ #x004 "User interrupt-enable register (urw)")
(defconstant utvec-csrΦ #x005 "User trap handler base address (urw)")

;; User Trap Handling
(defconstant uscratch-csrΦ #x040 "Scratch handler for user trap handlers (urw)")
(defconstant uepc-csrΦ #x041 "User exception program counter (urw)")
(defconstant ucause-csrΦ #x042 "User trap cause (urw)")
(defconstant ubadaddr-csrΦ #x043 "User bad address (urw)")
(defconstant utval-csrΦ #x043 "User bad address or instruction (urw)")
(defconstant uip-csrΦ #x044 "User interrupt pending (urw)")

;; User Floating-Point CSRs
(defconstant fflags-csrΦ #x001 "Floating-Point Accrued Exceptions (urw)")
(defconstant frm-csrΦ #x002 "Floating-Point Dynamic Rounding Mode (urw)")
(defconstant fcsr-csrΦ #x003 "Floating-Point Control and Status (frm + fflags) (urw)")

;; User Timers and Counters
(defconstant cycle-csrΦ #xC00 "Cycle counter (for RDCYCLE) (uro)")
(defconstant time-csrΦ #xC01 "Wall-clock time (for RDTIME) (uro)")
(defconstant instret-csrΦ #xC02 "Instructions-retired counter (for RDINSTRET) (uro)")
(defconstant hpmcounter3-csrΦ #xC03 "Performance-monitoring counter (mrw)")
(defconstant hpmcounter4-csrΦ #xC04 "Performance-monitoring counter (mrw)")
(defconstant hpmcounter5-csrΦ #xC05 "Performance-monitoring counter (mrw)")
(defconstant hpmcounter6-csrΦ #xC06 "Performance-monitoring counter (mrw)")
(defconstant hpmcounter7-csrΦ #xC07 "Performance-monitoring counter (mrw)")
(defconstant hpmcounter8-csrΦ #xC08 "Performance-monitoring counter (mrw)")
(defconstant hpmcounter9-csrΦ #xC09 "Performance-monitoring counter (mrw)")
(defconstant hpmcounter10-csrΦ #xC0A "Performance-monitoring counter (mrw)")
(defconstant hpmcounter11-csrΦ #xC0B "Performance-monitoring counter (mrw)")
(defconstant hpmcounter12-csrΦ #xC0C "Performance-monitoring counter (mrw)")
(defconstant hpmcounter13-csrΦ #xC0D "Performance-monitoring counter (mrw)")
(defconstant hpmcounter14-csrΦ #xC0E "Performance-monitoring counter (mrw)")
(defconstant hpmcounter15-csrΦ #xC0F "Performance-monitoring counter (mrw)")
(defconstant hpmcounter16-csrΦ #xC10 "Performance-monitoring counter (mrw)")
(defconstant hpmcounter17-csrΦ #xC11 "Performance-monitoring counter (mrw)")
(defconstant hpmcounter18-csrΦ #xC12 "Performance-monitoring counter (mrw)")
(defconstant hpmcounter19-csrΦ #xC13 "Performance-monitoring counter (mrw)")
(defconstant hpmcounter20-csrΦ #xC14 "Performance-monitoring counter (mrw)")
(defconstant hpmcounter21-csrΦ #xC15 "Performance-monitoring counter (mrw)")
(defconstant hpmcounter22-csrΦ #xC16 "Performance-monitoring counter (mrw)")
(defconstant hpmcounter23-csrΦ #xC17 "Performance-monitoring counter (mrw)")
(defconstant hpmcounter24-csrΦ #xC18 "Performance-monitoring counter (mrw)")
(defconstant hpmcounter25-csrΦ #xC19 "Performance-monitoring counter (mrw)")
(defconstant hpmcounter26-csrΦ #xC1A "Performance-monitoring counter (mrw)")
(defconstant hpmcounter27-csrΦ #xC1B "Performance-monitoring counter (mrw)")
(defconstant hpmcounter28-csrΦ #xC1C "Performance-monitoring counter (mrw)")
(defconstant hpmcounter29-csrΦ #xC1D "Performance-monitoring counter (mrw)")
(defconstant hpmcounter30-csrΦ #xC1E "Performance-monitoring counter (mrw)")
(defconstant hpmcounter31-csrΦ #xC1F "Performance-monitoring counter (mrw)")


(defconstant cycleh-csrΦ #xC80 "Upper 32 bits of cycle, RV32I only (uro)")
(defconstant timeh-csrΦ #xC81 "Upper 32 bits of time, RV32I only (uro)")
(defconstant instreth-csrΦ #xC82 "Upper 32 bits of instret, RV32I only (uro)")
(defconstant hpmcounter3h-csrΦ #xC83 "Upper 32 bits of hpmcounter3, RV32I only (mrw)")
(defconstant hpmcounter4h-csrΦ #xC84 "Upper 32 bits of hpmcounter4, RV32I only (mrw)")
(defconstant hpmcounter5h-csrΦ #xC85 "Upper 32 bits of hpmcounter5, RV32I only (mrw)")
(defconstant hpmcounter6h-csrΦ #xC86 "Upper 32 bits of hpmcounter6, RV32I only (mrw)")
(defconstant hpmcounter7h-csrΦ #xC87 "Upper 32 bits of hpmcounter7, RV32I only (mrw)")
(defconstant hpmcounter8h-csrΦ #xC88 "Upper 32 bits of hpmcounter8, RV32I only (mrw)")
(defconstant hpmcounter9h-csrΦ #xC89 "Upper 32 bits of hpmcounter9, RV32I only (mrw)")
(defconstant hpmcounter10h-csrΦ #xC8A "Upper 32 bits of hpmcounter10, RV32I only (mrw)")
(defconstant hpmcounter11h-csrΦ #xC8B "Upper 32 bits of hpmcounter11, RV32I only (mrw)")
(defconstant hpmcounter12h-csrΦ #xC8C "Upper 32 bits of hpmcounter12, RV32I only (mrw)")
(defconstant hpmcounter13h-csrΦ #xC8D "Upper 32 bits of hpmcounter13, RV32I only (mrw)")
(defconstant hpmcounter14h-csrΦ #xC8E "Upper 32 bits of hpmcounter14, RV32I only (mrw)")
(defconstant hpmcounter15h-csrΦ #xC8F "Upper 32 bits of hpmcounter15, RV32I only (mrw)")
(defconstant hpmcounter16h-csrΦ #xC90 "Upper 32 bits of hpmcounter16, RV32I only (mrw)")
(defconstant hpmcounter17h-csrΦ #xC91 "Upper 32 bits of hpmcounter17, RV32I only (mrw)")
(defconstant hpmcounter18h-csrΦ #xC92 "Upper 32 bits of hpmcounter18, RV32I only (mrw)")
(defconstant hpmcounter19h-csrΦ #xC93 "Upper 32 bits of hpmcounter19, RV32I only (mrw)")
(defconstant hpmcounter20h-csrΦ #xC94 "Upper 32 bits of hpmcounter20, RV32I only (mrw)")
(defconstant hpmcounter21h-csrΦ #xC95 "Upper 32 bits of hpmcounter21, RV32I only (mrw)")
(defconstant hpmcounter22h-csrΦ #xC96 "Upper 32 bits of hpmcounter22, RV32I only (mrw)")
(defconstant hpmcounter23h-csrΦ #xC97 "Upper 32 bits of hpmcounter23, RV32I only (mrw)")
(defconstant hpmcounter24h-csrΦ #xC98 "Upper 32 bits of hpmcounter24, RV32I only (mrw)")
(defconstant hpmcounter25h-csrΦ #xC99 "Upper 32 bits of hpmcounter25, RV32I only (mrw)")
(defconstant hpmcounter26h-csrΦ #xC9A "Upper 32 bits of hpmcounter26, RV32I only (mrw)")
(defconstant hpmcounter27h-csrΦ #xC9B "Upper 32 bits of hpmcounter27, RV32I only (mrw)")
(defconstant hpmcounter28h-csrΦ #xC9C "Upper 32 bits of hpmcounter28, RV32I only (mrw)")
(defconstant hpmcounter29h-csrΦ #xC9D "Upper 32 bits of hpmcounter29, RV32I only (mrw)")
(defconstant hpmcounter30h-csrΦ #xC9E "Upper 32 bits of hpmcounter30, RV32I only (mrw)")
(defconstant hpmcounter31h-csrΦ #xC9F "Upper 32 bits of hpmcounter31, RV32I only (mrw)")

;; Supervisor Trap Setup
(defconstant sstatus-csrΦ #x100 "Supervisor status register (srw)")
(defconstant sedeleg-csrΦ #x102 "Supervisor exception delegation register (srw)")
(defconstant sideleg-csrΦ #x103 "Supervisor interrupt delegation register (srw)")
(defconstant sie-csrΦ #x104 "Supervisor interrupt-enable register (src)")
(defconstant stvec-csrΦ #x105 "Supervisor trap handler base address (srw)")
(defconstant scounteren-csrΦ #x106 "Supervisor counter enable (swr)")

;; Supervisor Trap Handling
(defconstant sscratch-csrΦ #x140 "Scratch register for supervisor trap handlers (srw)")
(defconstant sepc-csrΦ #x141 "Supervisor exception program counter (srw)")
(defconstant scause-csrΦ #x142 "Supervisor trap cause (srw)")
(defconstant sbadaddr-csrΦ #x143 "Supervisor bad address (srw)")
(defconstant stval-csrΦ #x143 "Supervisor bad address or instruction (srw)")
(defconstant sip-csrΦ #x144 "Supervisor interrupt pending (srw)")

;; Supervisor Protection and Translation
(defconstant sptbr-csrΦ #x180 "Page-table base register (srw)")
(defconstant satp-csrΦ #x180 "Supervisor address translation and protection (srw)")

;; ;; Supervisor Timers and Counters
;; ;; Could not find these in 2019 draft??
;; (defconstant scycle-csrΦ #xD00 "Supervisor cycle counter (sro)")
;; (defconstant stime-csrΦ #xD01 "Supervisor wall-clock time (sro)")
;; (defconstant sinstret-csrΦ #xD02 "Supervisor instructions-retired counter (sro)")
;; (defconstant scycleh-csrΦ #xD80 "Upper 32 bits of scycle, RV32I only (sro)")
;; (defconstant stimeh-csrΦ #xD81 "Upper 32 bits of stime, RV32I only (sro)")
;; (defconstant sinstreth-csrΦ #xD82 "Upper 32 bits of sinstret, RV32I only (sro)")

;; Hypervisor no longer in use
;; ;; Hypervisor Trap Setup
;; (defconstant hstatus-csrΦ #x200 "Hypervisor status register (hrw)")
;; (defconstant hedeleg-csrΦ #x202 "Hypervisor exception delegation register (mrw)")
;; (defconstant hideleg-csrΦ #x203 "Hypervisor interrupt delegation register (mrw)")
;; (defconstant hie-csrΦ #x204 "Hypervisor interrupt-enable register (mrw)")
;; (defconstant htvec-csrΦ #x205 "Hypervisor trap handler base address (hrw)")

;; ;; Hypervisor Trap Handling
;; (defconstant hscratch-csrΦ #x240 "Scratch register for hypervisor trap handlers (hrw)")
;; (defconstant hepc-csrΦ #x241 "Hypervisor exception program counter (hrw)")
;; (defconstant hcause-csrΦ #x242 "Hypervisor trap cause (hrw)")
;; (defconstant hbadaddr-csrΦ #x243 "Hypervisor bad address (hrw)")
;; (defconstant hip-csrΦ #x244 "Hypervisor interrupt pending (hrw)")

;; ;; Hypervisor Protection and Translation

;; ;; Hypervisor Timers and Counters
;; (defconstant hcycle-csrΦ #xE00 "Hypervisor cycle counter (sro)")
;; (defconstant htime-csrΦ #xE01 "Hypervisor wall-clock time (sro)")
;; (defconstant hinstret-csrΦ #xE02 "Hypervisor instructions-retired counter (sro)")
;; (defconstant hcycleh-csrΦ #xE80 "Upper 32 bits of hcycle, RV32I only (sro)")
;; (defconstant htimeh-csrΦ #xE81 "Upper 32 bits of htime, RV32I only (sro)")
;; (defconstant hinstreth-csrΦ #xE82 "Upper 32 bits of hinstret, RV32I only (sro)")

;; Machine Information Registers
(defconstant mvendorid-csrΦ #xF11 "Vendor ID (mro)")
(defconstant marchid-csrΦ #xF12 "Architecture ID (mro)")
(defconstant mimpid-csrΦ #xF13 "Implementation ID (mro)")
(defconstant mhartid-csrΦ #xF14 "Hardware thread ID (mro)")

;; Machine Trap Setup
(defconstant mstatus-csrΦ #x300 "Machine status register (mrw)")
(defconstant misa-csrΦ #x301 "ISA and extensions supported (mrw)")
(defconstant medeleg-csrΦ #x302 "Machine exception delegation register (mrw)")
(defconstant mideleg-csrΦ #x303 "Machine interrupt delegation register (mrw)")
(defconstant mie-csrΦ #x304 "Machine interrupt-enable register (mrw)")
(defconstant mtvec-csrΦ #x305 "Machine trap-handler base address (mrw)")
;; Only exists if there is user mode bit 0 - CY, bit 1 - TM, bit 2 - IR
(defconstant mcounteren-csrΦ #x306 "Machine counter enable (mrw)")

;; Machine Trap Handling
(defconstant mscratch-csrΦ #x340 "Scratch register for machine trap handlers (mrw)")
(defconstant mepc-csrΦ #x341 "Machine exception program counter (mrw)")
(defconstant mcause-csrΦ #x342 "Machine trap cause (mrw)")
(defconstant mbadaddr-csrΦ #x343 "Machine bad address (mrw)")
(defconstant mtval-csrΦ #x343 "Machine bad address or instruction (mrw)")
(defconstant mip-csrΦ #x344 "Machine interrupt pending (mrw)")

;; Machine Protection and translation
(defconstant pmpcfg0-csrΦ #x3A0 "Physical memory protection configuration (mrw)")
(defconstant pmpcfg1-csrΦ #x3A1 "Physical memory protection configuration (RV32 only) (mrw)")
(defconstant pmpcfg2-csrΦ #x3A2 "Physical memory protection configuration (mrw)")
(defconstant pmpcfg3-csrΦ #x3A3 "Physical memory protection configuration (RV32 only) (mrw)")
(defconstant pmpaddr0-csrΦ #x3B0 "Physical memory protection address register (mrw)")
(defconstant pmpaddr1-csrΦ #x3B1 "Physical memory protection address register (mrw)")
(defconstant pmpaddr2-csrΦ #x3B2 "Physical memory protection address register (mrw)")
(defconstant pmpaddr3-csrΦ #x3B3 "Physical memory protection address register (mrw)")
(defconstant pmpaddr4-csrΦ #x3B4 "Physical memory protection address register (mrw)")
(defconstant pmpaddr5-csrΦ #x3B5 "Physical memory protection address register (mrw)")
(defconstant pmpaddr6-csrΦ #x3B6 "Physical memory protection address register (mrw)")
(defconstant pmpaddr7-csrΦ #x3B7 "Physical memory protection address register (mrw)")
(defconstant pmpaddr8-csrΦ #x3B8 "Physical memory protection address register (mrw)")
(defconstant pmpaddr9-csrΦ #x3B9 "Physical memory protection address register (mrw)")
(defconstant pmpaddr10-csrΦ #x3BA "Physical memory protection address register (mrw)")
(defconstant pmpaddr11-csrΦ #x3BB "Physical memory protection address register (mrw)")
(defconstant pmpaddr12-csrΦ #x3BC "Physical memory protection address register (mrw)")
(defconstant pmpaddr13-csrΦ #x3BE "Physical memory protection address register (mrw)")
(defconstant pmpaddr14-csrΦ #x3BD "Physical memory protection address register (mrw)")
(defconstant pmpaddr15-csrΦ #x3BF "Physical memory protection address register (mrw)")

;; ;; Machine Protection and Translation
;; ;; Could not find these in 2019 draft??
;; (defconstant mbase-csrΦ #x380 "Base register (mrw)")
;; (defconstant mbound-csrΦ #x381 "Bound register (mrw)")
;; (defconstant mibase-csrΦ #x382 "Instruction base register (mrw)")
;; (defconstant mibound-csrΦ #x383 "Instruction bound register (mrw)")
;; (defconstant mdbase-csrΦ #x384 "Data base register (mrw)")
;; (defconstant mdbound-csrΦ #x385 "Data bound register (mrw)")

;; Machine Counters / Timers
(defconstant mcycle-csrΦ #xB00 "Machine cycle counter (mrw)")
(defconstant mtime-csrΦ #xB01 "Machine wall-clock time (mrw)")
(defconstant minstret-csrΦ #xB02 "Machine instructions-retired counter (mrw)")
(defconstant mhpmcounter3-csrΦ #xB03 "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter4-csrΦ #xB04 "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter5-csrΦ #xB05 "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter6-csrΦ #xB06 "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter7-csrΦ #xB07 "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter8-csrΦ #xB08 "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter9-csrΦ #xB09 "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter10-csrΦ #xB0A "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter11-csrΦ #xB0B "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter12-csrΦ #xB0C "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter13-csrΦ #xB0D "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter14-csrΦ #xB0E "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter15-csrΦ #xB0F "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter16-csrΦ #xB10 "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter17-csrΦ #xB11 "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter18-csrΦ #xB12 "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter19-csrΦ #xB13 "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter20-csrΦ #xB14 "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter21-csrΦ #xB15 "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter22-csrΦ #xB16 "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter23-csrΦ #xB17 "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter24-csrΦ #xB18 "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter25-csrΦ #xB19 "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter26-csrΦ #xB1A "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter27-csrΦ #xB1B "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter28-csrΦ #xB1C "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter29-csrΦ #xB1D "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter30-csrΦ #xB1E "Machine performance-monitoring counter (mrw)")
(defconstant mhpmcounter31-csrΦ #xB1F "Machine performance-monitoring counter (mrw)")


(defconstant mcycleh-csrΦ #xB80 "Upper 32 bits of mcycle, RV32I only (mrw)")
(defconstant mtimeh-csrΦ #xB81 "Upper 32 bits of mtime, RV32I only (mrw)")
(defconstant minstreth-csrΦ #xB82 "Upper 32 bits of minstret, RV32I only (mrw)")
(defconstant mhpmcounter3h-csrΦ #xB83 "Upper 32 bits of mhpmcounter3, RV32I only (mrw)")
(defconstant mhpmcounter4h-csrΦ #xB84 "Upper 32 bits of mhpmcounter4, RV32I only (mrw)")
(defconstant mhpmcounter5h-csrΦ #xB85 "Upper 32 bits of mhpmcounter5, RV32I only (mrw)")
(defconstant mhpmcounter6h-csrΦ #xB86 "Upper 32 bits of mhpmcounter6, RV32I only (mrw)")
(defconstant mhpmcounter7h-csrΦ #xB87 "Upper 32 bits of mhpmcounter7, RV32I only (mrw)")
(defconstant mhpmcounter8h-csrΦ #xB88 "Upper 32 bits of mhpmcounter8, RV32I only (mrw)")
(defconstant mhpmcounter9h-csrΦ #xB89 "Upper 32 bits of mhpmcounter9, RV32I only (mrw)")
(defconstant mhpmcounter10h-csrΦ #xB8A "Upper 32 bits of mhpmcounter10, RV32I only (mrw)")
(defconstant mhpmcounter11h-csrΦ #xB8B "Upper 32 bits of mhpmcounter11, RV32I only (mrw)")
(defconstant mhpmcounter12h-csrΦ #xB8C "Upper 32 bits of mhpmcounter12, RV32I only (mrw)")
(defconstant mhpmcounter13h-csrΦ #xB8D "Upper 32 bits of mhpmcounter13, RV32I only (mrw)")
(defconstant mhpmcounter14h-csrΦ #xB8E "Upper 32 bits of mhpmcounter14, RV32I only (mrw)")
(defconstant mhpmcounter15h-csrΦ #xB8F "Upper 32 bits of mhpmcounter15, RV32I only (mrw)")
(defconstant mhpmcounter16h-csrΦ #xB90 "Upper 32 bits of mhpmcounter16, RV32I only (mrw)")
(defconstant mhpmcounter17h-csrΦ #xB91 "Upper 32 bits of mhpmcounter17, RV32I only (mrw)")
(defconstant mhpmcounter18h-csrΦ #xB92 "Upper 32 bits of mhpmcounter18, RV32I only (mrw)")
(defconstant mhpmcounter19h-csrΦ #xB93 "Upper 32 bits of mhpmcounter19, RV32I only (mrw)")
(defconstant mhpmcounter20h-csrΦ #xB94 "Upper 32 bits of mhpmcounter20, RV32I only (mrw)")
(defconstant mhpmcounter21h-csrΦ #xB95 "Upper 32 bits of mhpmcounter21, RV32I only (mrw)")
(defconstant mhpmcounter22h-csrΦ #xB96 "Upper 32 bits of mhpmcounter22, RV32I only (mrw)")
(defconstant mhpmcounter23h-csrΦ #xB97 "Upper 32 bits of mhpmcounter23, RV32I only (mrw)")
(defconstant mhpmcounter24h-csrΦ #xB98 "Upper 32 bits of mhpmcounter24, RV32I only (mrw)")
(defconstant mhpmcounter25h-csrΦ #xB99 "Upper 32 bits of mhpmcounter25, RV32I only (mrw)")
(defconstant mhpmcounter26h-csrΦ #xB9A "Upper 32 bits of mhpmcounter26, RV32I only (mrw)")
(defconstant mhpmcounter27h-csrΦ #xB9B "Upper 32 bits of mhpmcounter27, RV32I only (mrw)")
(defconstant mhpmcounter28h-csrΦ #xB9C "Upper 32 bits of mhpmcounter28, RV32I only (mrw)")
(defconstant mhpmcounter29h-csrΦ #xB9D "Upper 32 bits of mhpmcounter29, RV32I only (mrw)")
(defconstant mhpmcounter30h-csrΦ #xB9E "Upper 32 bits of mhpmcounter30, RV32I only (mrw)")
(defconstant mhpmcounter31h-csrΦ #xB9F "Upper 32 bits of mhpmcounter31, RV32I only (mrw)")

;; Machine Counter Setup
;; These three taken out 1.9-1
;; (defconstant mucounteren-csrΦ #x320 "Machine interrupt-enable register (mrw)")
;; (defconstant mscounteren-csrΦ #x321 "Supervisor-mode counter enable (mrw)")
;; (defconstant mhcounteren-csrΦ #x322 "Hypervisor-mode counter enable (mrw)")
(defconstant mhpmevent3-csrΦ #x323 "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent4-csrΦ #x324 "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent5-csrΦ #x325 "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent6-csrΦ #x326 "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent7-csrΦ #x327 "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent8-csrΦ #x328 "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent9-csrΦ #x329 "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent10-csrΦ #x32A "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent11-csrΦ #x32B "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent12-csrΦ #x32C "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent13-csrΦ #x32D "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent14-csrΦ #x32E "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent15-csrΦ #x32F "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent16-csrΦ #x330 "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent17-csrΦ #x331 "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent18-csrΦ #x332 "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent19-csrΦ #x333 "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent20-csrΦ #x334 "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent21-csrΦ #x335 "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent22-csrΦ #x336 "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent23-csrΦ #x337 "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent24-csrΦ #x338 "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent25-csrΦ #x339 "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent26-csrΦ #x33A "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent27-csrΦ #x33B "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent28-csrΦ #x33C "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent29-csrΦ #x33D "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent30-csrΦ #x33E "Machine performance-monitoring event selector (mrw)")
(defconstant mhpmevent31-csrΦ #x33F "Machine performance-monitoring event selector (mrw)")

;; Debug/Trace Registers (shared with Debug Mode)
(defconstant tselect-csrΦ #x7A0 "Debug/Trace trigger register select (mrw)")
(defconstant tdata1-csrΦ #x7A1 "First Debug/Trace trigger data register (mrw)")
(defconstant tdata2-csrΦ #x7A2 "Second Debug/Trace trigger data register (mrw)")
(defconstant tdata3-csrΦ #x7A3 "Third Debug/Trace trigger data register (mrw)")

;; Debug Mode Registers
(defconstant dcsr-csrΦ #x7B0 "Debug control and status register (mrw)")
(defconstant dpc-csrΦ #x7B1 "Debug program counter (mrw)")
(defconstant dscratch-csrΦ #x7B2 "Debug scratch register (mrw)")

;; Nonstandard
(defconstant mtohost-csrΦ #x780 "IO to Host (mrw)")
(defconstant mfromhost-csrΦ #x781 "IO from Host (mrw)")
(defconstant mreset-csrΦ #x782 "Reset (mrw)")
(defconstant mipi-csrΦ #x783 "Inter Processor Interrupt (mrw)")
(defconstant miobase-csrΦ #x784 "IO Base (mrw)")

;; Not in standard?
(defconstant csr-mtvtΦ #x307)
(defconstant csr-mnxtiΦ #x345)


(defconstant csr-mtvt2Φ #x7ec)
(defconstant csr-jalmnxtiΦ #x7ed)
(defconstant csr-pushmcauseΦ #x7ee)
(defconstant csr-pushmepcΦ #x7ef)
(defconstant csr-pushmsubmΦ #x7eb)

(defconstant csr-wfeΦ #x810) ;;
(defconstant csr-sleepvalueΦ #x811) ;;
(defconstant csr-txevtΦ #x812) ;;

(defconstant csr-mcountinhibitΦ #x320 "Used to stop mcount. Bit0 - cycles, Bit2 - instret")
(defconstant csr-mmisc-ctlΦ #x7d0) ;;
(defconstant csr-mnvecΦ #x7c3)
(defconstant csr-msubmΦ #x7c4) ;;


        ;;;; csr-utility functions ;;;;

(defun rdtime (reg)
  (csrr reg time-csrΦ))

(defun rdtimeh (reg)
  (csrr reg timeh-csrΦ))

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
