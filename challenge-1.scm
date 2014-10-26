; http://cryptopals.com/sets/1/challenges/1/
; Convert hex to base64

(define (safe-string-head s start) ; gives 'up to' 6 or remaining
  (let ((sl (string-length s)))
    (if (>= sl start)
        (string-head s start)
        (string-head s sl)))) 

(define (safe-string-tail s end) ; gives 'up to' 6 and remaining
  (let ((sl (string-length s)))
    (if (>= sl end)
        (string-tail s end)
        (string-tail s sl)))) ; 

(define (string-split s split-at)
  (define (split ss ac)
    (if (string-null? ss)
        ac
        (split (safe-string-tail ss split-at) (cons (safe-string-head ss split-at) ac))))
  (reverse (split s '())))

; expects 6 character hex string and returns 24 character binary string
(define (hex-string->binary-string hs)
  (let ((hs-lst (string-split hs 2))
        ; pads to length of 8 as conversion drops leading zero
        (p (lambda (x) (string-pad-left (number->string (string->number x 16) 2) 8 #\0))))
    (fold-right (lambda (x r) (string-append (p x) r)) "" hs-lst)))

; maps a 24-bit binary string to a list of 4 decimals
(define (binary-string->decimals bs) 
  ; split out binary string into 4 6-bit strings
  (let ((bs-lst (string-split bs 6))
        ; converts binary string (padding with zeros if less than 6 bits) into decimal
        (p (lambda (s) (string->number (string-pad-right (safe-string-head s 6) 6 #\0) 2))))
    ; apply conversion to each string
    (map p bs-lst)))

; maps a decimal value to its base64 equivalent
(define (decimal->base64 d)
  (let ((base64-table "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))
    (char->name (string-ref base64-table d))))

(define (base64-encode hex-string)
  ; split out hex string into 24-bit binary strings
  (let ((bs-lst (map hex-string->binary-string (string-split hex-string 6))))
    ; pad resulting text
    (define (pad-base64 s)
      (let ((mod (modulo (string-length hex-string) 6)))
        (if (= mod 0) 
            s ; full 'frame', no padding needed
            (string-append s (make-string (/ (- 6 mod) 2) #\=))))) ; partial 'frame', pad with 1 or 2 '='s
    (pad-base64 (apply string (map decimal->base64 (concatenate (map binary-string->decimals bs-lst)))))))

; Perform conversion
(define hex "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")
(define base64 "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")
(define binary "010010010010011101101101001000000110101101101001011011000110110001101001011011100110011100100000011110010110111101110101011100100010000001100010011100100110000101101001011011100010000001101100011010010110101101100101001000000110000100100000011100000110111101101001011100110110111101101110011011110111010101110011001000000110110101110101011100110110100001110010011011110110111101101101")
(define dec (list 18 18 29 45 8 6 45 41 27 6 49 41 27 38 28 32 30 22 61 53 28 34 1 34 28 38 5 41 27 34 1 44 26 22 45 37 8 6 4 32 28 6 61 41 28 54 61 46 27 55 21 51 8 6 53 53 28 54 33 50 27 54 61 45))
; Also, "I'm killing your brain like a poisonous mushroom"

(define (test e a)
  (display "Expected and actual values")
  (display (if (equal? a e) " are " " are NOT "))
  (display "equal.")
  (newline)
  (display (list "Expected:" e " Actual:" a))
  (newline)
  (equal? a e))

(define gBinary (hex-string->binary-string hex))
;(display (equal? binary gBinary))
(test binary gBinary)
(define gDec (binary-string->decimals gBinary))
;(display (equal? dec gDec))
(test dec gDec)
(define gBase64 (base64-encode hex))
;(display (equal? base64 gBase64))
(test base64 gBase64)

(define maryHex "4D61727920686164")
(define maryBinary "0100110101100001011100100111100100100000011010000110000101100100")
; Also, "Mary had"
(define maryDec (list 19 22 05 50 30 18 01 40 24 22 16))
(define maryBase64 "TWFyeSBoYWQ=")

(define gMaryBinary (hex-string->binary-string maryHex))
;(display (equal? maryBinary gMaryBinary))
(test maryBinary gMaryBinary)
(define gMaryDec (binary-string->decimals maryBinary))
;(display (equal? maryDec gMaryDec))
(test maryDec gMaryDec)
(define gMaryBase64 (base64-encode maryHex))
;(display (equal? maryBase64 gMaryBase64))
(test maryBase64 gMaryBase64)
