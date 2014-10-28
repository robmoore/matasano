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
; How? Devise some method for "scoring" a piece of English plaintext. Character frequency is a good metric. Evaluate each output and choose the one with the best score.

(load "challenge-2.scm")

(define encoded "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")

; Look for presence of 'e' above 10%
; Walk through all characters and map percent of 'e's in output
; Sort by highest value

; map character to xor output
(define (apply-xor bit-strings key-bit-string)
  (map (lambda (x) (bit-string-xor x key-bit-string)) bit-strings)))

(define (apply-xor-with-char bit-strings key-char)
  (let ((key-bit-string (unsigned-integer->bit-string 8 (char->ascii key-char))))
    (apply-xor bit-strings key-bit-string)))

(define (bit-strings->ascii-string bs)
  (apply string (map ascii->char (map bit-string->unsigned-integer bs))))

;    (apply string (map ascii->char (map bit-string->unsigned-integer xord))))
;(define xor-map (map xor-decode (map lambdaapply-xor-with-char (string->list ascii-chars)))

; fun take bit-strings

(define (encoded-bit-strings->ascii-string bit-strings key-char)
  (let ((decoded-bit-strings (apply-xor-with-char bit-strings key-char)))
    (bit-strings->ascii-string decoded-bit-strings)))

(define (xor-map)
  (let ((ascii-chars (char-set-members (ascii-range->char-set 0 127)))
        ; Note: Use of reverse probably means we need to revisit bit-string-split
        (encoded-bit-strings (reverse (bit-string-split (hex-string->bit-string encoded) 8))))
    (map (lambda (x) (encoded-bit-strings->ascii-string encoded-bit-strings x)) ascii-chars)))

; create a function that goes through all characters in a given set and provides a count for each incident of these characters
; namely, vowels and perhaps t's
  
;sort sequence procedure. for this to work we need to create a pair (char-key : decoded string)

(define wiki "57696b69")

; encode wiki using 243 

; NOTE: Use of reverse
(define wiki-bit-strings (reverse (bit-string-split (hex-string->bit-string wiki) 8)))

(apply-xor-with-char wiki-binary-strings (ascii->char 243))



