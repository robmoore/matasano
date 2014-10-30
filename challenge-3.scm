; http://cryptopals.com/sets/1/challenges/3/
; Single-byte XOR cipher

; The hex encoded string:
;
;  1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736
;
; ... has been XOR'd against a single character. Find the key, decrypt the message.
;
; You can do this by hand. But don't: write code to do it for you.
;
; How? Devise some method for "scoring" a piece of English plaintext. Character frequency 
; is a good metric. Evaluate each output and choose the one with the best score.
;
; Very helpful: http://en.wikipedia.org/wiki/XOR_cipher

(load "challenge-2.scm")

(define encoded "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")
(define encoded-with-W "1438383c3e3930771a147024773b3e3c327736772738223933773831773536343839")

; map character to xor output
(define (apply-xor bit-strings key-bit-string)
  (map (lambda (bs) (bit-string-xor bs key-bit-string)) bit-strings))

(define (apply-xor-with-char bit-strings key-char)
  (let ((key-bit-string (unsigned-integer->bit-string 8 (char->ascii key-char))))
    (apply-xor bit-strings key-bit-string)))

(define (bit-strings->ascii-string bs)
  (apply string (map ascii->char (map bit-string->unsigned-integer bs))))

(define (ascii-string->bit-strings s)
  (let ((ascii-chars (map char->ascii (string->list s))))
    (map (lambda (c) (unsigned-integer->bit-string 8 c)) ascii-chars)))

(define (encoded-bit-strings->ascii-string bit-strings key-char)
  (let ((decoded-bit-strings (apply-xor-with-char bit-strings key-char)))
    (bit-strings->ascii-string decoded-bit-strings)))

(define (freq-percentage s)
  (let ((etaoin-char-set (string->char-set "ETAOIN SHRDLU")))
    (define (percentage total)
      (let ((sl (string-length s)))
        (if (or (= total 0) (= sl 0))
            0
            (exact->inexact (/ total (string-length s))))))
    (define (count chars total)
      (if (null? chars)
          total
          (count (cdr chars) (if (char-set-member? etaoin-char-set (car chars)) (+ total 1) total))))
    (percentage (count (string->list (string-upcase s)) 0))))

(define (xor-results encoded-string)
  (let ((ascii-chars (char-set-members (ascii-range->char-set 0 127)))
        (encoded-bit-strings (bit-string-split (hex-string->bit-string encoded-string) 8)))
    (define (make-list c)
      (let ((decoded-string (encoded-bit-strings->ascii-string encoded-bit-strings c)))
        (list c (freq-percentage decoded-string) decoded-string)))
    (map make-list ascii-chars)))

(define (encode string key-char)
  (let ((bit-strings (ascii-string->bit-strings string)))
    (define xor-results (apply-xor-with-char bit-strings key-char))
    (bit-strings->hex-string xor-results)))

(define (decode encoded-string)
  (let ((results (xor-results encoded-string)))
    (define (comparator x y)
      (let ((x-score (second x)) (y-score (second y)))
        (>= x-score y-score)))
    (if (string-null? encoded-string)
        (error "Cannot decode empty string!")
        ; Format of entry is (key, frequency score, decoded text)
        (last (first (sort results comparator))))))

(define (decode-with-key encoded-string key-char)
  ; convert to bit-strings from hex
  (let ((bit-strings (bit-string-split (hex-string->bit-string encoded-string) 8)))
    (define xor-results (apply-xor-with-char bit-strings key-char))
    (bit-strings->ascii-string xor-results)))

(define decoded (decode-with-key encoded-with-W #\W))
(equal? (decode encoded) decoded)

