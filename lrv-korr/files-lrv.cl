
                    ;;;;;;;; file manipulation for the Risc-V assembler ;;;;;;;;

(in-package "RVASM")

;; An assembler creates a file containing machine code that can be loaded or run
;; on a paticular machine. The functions in this file focus on creating such files
;; from the final vector produced and also reading from files to a vector.



        ;;;; Writing files from vector  ;;;;



;; Write a sequence of bytes from a vector to a file in raw binary format
(defun write-binary-file (filename vector &key (if-exists :supersede)
                                               (external-format :default)
                                            (element-type '(unsigned-byte 8)))
"Write a sequence of bytes from a vector to a file in raw binary format"
  (with-open-file (out filename :if-exists if-exists :element-type element-type
                                :external-format external-format
                                :direction :output)
    (write-sequence vector out)))


;; TODO not sure what this does yet!!!
;; Seems to make it possible to do "(setf (bin-file "file" ) vector)"
;; to set "file" to vector, basically restructures the syntax to be more
;; readable/intuitive
(defsetf bin-file (filename &rest args) (sequence)
 #+NIL  `(apply 'write-binary-file ,filename ,sequence ,args)
  ;; Doesn't work on CCL:
  #+ECL `(write-binary-file ,filename ,sequence ,@args))


        ;;;; Reading files from file to vector;;;;


(defun read-binary-file (filename &key (element-type '(unsigned-byte 8)))
  "Read a sequence of raw bytes from a file to a vector"
  (with-open-file (in filename :element-type element-type)
    (let ((data (make-array (file-length in))))
      (read-sequence data in)
      data)))
