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

(load "common.scm")

(define encoded "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")
(define encoded-with-W "1438383c3e3930771a147024773b3e3c327736772738223933773831773536343839")

(define decoded (decode-with-key encoded-with-W #\W))
(equal? (last (decode-without-key encoded)) decoded)

