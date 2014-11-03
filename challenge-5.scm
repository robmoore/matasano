; Implement repeating-key XOR
;
; Here is the opening stanza of an important work of the English language:
;
; Burning 'em, if you ain't quick and nimble
; I go crazy when I hear a cymbal
;
; Encrypt it, under the key "ICE", using repeating-key XOR.
;
; In repeating-key XOR, you'll sequentially apply each byte of the key; the 
; first byte of plaintext will be XOR'd against I, the next C, the next E, 
; then I again for the 4th byte, and so on.
;
; It should come out to:
;
; 0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272
; a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f
;
; Encrypt a bunch of stuff using your repeating-key XOR function. Encrypt your
; mail. Encrypt your password file. Your .sig file. Get a feel for it. I promise, 
; we aren't wasting your time with this.

(load "common.scm")

(define decoded-stanza "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal")

(define encoded-stanza-line-1 "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272")
(define encoded-stanza-line-2 "a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f")
(define encoded-stanza (string-append encoded-stanza-line-1 encoded-stanza-line-2))

(define (char->bit-string char)
  (unsigned-integer->bit-string 8 (char->ascii char)))

(define (encode-with-repeating-key string key-chars)
  (define (enc bs ks acc)
    (if (null? bs)
        acc
        (enc (cdr bs) (cdr ks) (cons (bit-string-xor (car bs) (car ks)) acc))))
  (let ((bit-strings (ascii-string->bit-strings string))
        (key-bit-strings (apply circular-list (map char->bit-string key-chars))))
    (bit-strings->hex-string (reverse (enc bit-strings key-bit-strings '())))))

(define (decode-with-repeating-key string key-chars)
  (define (dec bs ks acc)
    (if (null? bs)
        acc
        (dec (cdr bs) (cdr ks) (cons (bit-string-xor (car bs) (car ks)) acc))))
  (let ((bit-strings (hex-string->bit-strings string))
        (key-bit-strings (apply circular-list (map char->bit-string key-chars))))
    (bit-strings->ascii-string (reverse (dec bit-strings key-bit-strings '())))))

(define repeating-key (string->list "ICE"))

(equal? (encode-with-repeating-key decoded-stanza repeating-key) encoded-stanza)              
(equal? (decode-with-repeating-key encoded-stanza repeating-key) decoded-stanza)              
