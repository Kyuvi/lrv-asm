
(in-package :gd32rv)


(let ((*env* (make-instance 'basic-env :address 40 )))
  (and 'x0 'x4 'x7)
  (sub 'x0 'x4 'x7)
  (addi 'x0 'x4 40)
  (jal 'x0 (label 'tee))
  ;; (jalr 'x4 'x7 (label 'tee))
  (set-label 'tuolla)
  (andi 'x0 'x4 20)
  (bne 'x0 'x4 (label 'tuolla))
  (set-label 'tee)

 (setf (bin-file "00-test.bin") (link *env*))
  )
