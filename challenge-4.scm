; http://cryptopals.com/sets/1/challenges/4/
; Detect single-character XOR
;
; One of the 60-character strings in this file (4.txt) has been encrypted by single-character XOR.
; 
; Find it.
; 
; (Your code from #3 should help.)

(load "common.scm")

(define encoded-with-W "19382077233f362377233f3277273625232e773e24773d223a273e39305d")

(define (find-xor-encoded-string candidates)
  (let ((xor-results (map (lambda (candidate) (decode-without-key candidate)) candidates)))
    ; Return the most likely result
    (first (sort xor-results make-xor-results-frequency-comparator))))

(define decoded (decode-with-key encoded-with-W #\W))
(define candidates (read-file "4.txt"))
(equal? (last (find-xor-encoded-string candidates)) decoded)