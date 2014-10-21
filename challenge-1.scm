; http://cryptopals.com/sets/1/challenges/1/
; Convert hex to base64

; TODO: If going from hex to decimal could we just use #x<hex value> to get decimal direcly?
; Need to step through the process here

; TODO: Use accumulator?
; Convert from hex to bytes (should be 6 characters long in and 8 characters out)
(define (hexadecimal-string->binary-string x)
  (define (hexadecimal-string->integer b) (string->number b 16))
  (string-append (string-pad-left (number->string (hexadecimal-string->integer (string-head x 2)) 2) 8 #\0) 
                 (if (> (string-length x) 2) (hexadecimal-string->binary-string (string-tail x 2)) "")))

; TODO: Use accumulator?
(define (binary-string->decimal-list bs)
  (define (string-head-max s) ; gives 'up to' 6 and pads the rest with 0s
    (if (>= (string-length s) 6)
        (string-head s 6)
        (string-pad-right s 6 #\0)))
  (define (string-tail-max s)
    (if (>= (string-length s) 6)
        (string-tail s 6)
        (string-tail s (string-length s))))
  ; take string of binary with length 24 and convert to integer value
  ; if bytes less than 24 then 
  (define (binary-string->decimal x) (string->number(string-pad-left x 24 #\0) 2))
  (if (= (string-length bs) 0)
      '()
      (append (list (binary-string->decimal (string-head-max bs))))))

; Read 6 characters at a time from hex string
; convert to bytes
; take 6 of 24 3x 
; if 16 or 8, convert 1st or 2nd value but sub in = in case where missing
; convert to int each time
; look up base64 character
; append to string
; TODO: Use accumulator?
(define (base64-encode x)
  (let ((base64-table "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
        (binary-string (hexadecimal-string->binary-string x)))
    
    (define (equalSigns)
      (let ((mod (modulo (string-length binary-string) 24)))
        (if (= mod 0) "" (make-string (/ (- 24 mod) 8) #\=))))   ; map binary into decimal
    
    (define (decimal->base64 d)
      (char->name(string-ref base64-table d)))
    
    (define (decimal->base64-character i)
      (string-ref base64-table i))
    
    (define (decimal-list->base64 dl)
      (define head (first dl))
      (define tail (list-tail dl 1))
      (if (null? tail)
          (string-append (decimal->base64 head))
          (string-append (decimal->base64 head) (decimal-list->base64 tail))))
    
    ; Now the framework is in place, perform encoding.
    (define decimal-list (binary-string->decimal-list binary-string))
    (if (null? decimal-list) "" (string-append (decimal-list->base64 decimal-list) (equalSigns)))))

; Perform conversion
(define hex "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")
(define base64 "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")
(define binary "010010010010011101101101001000000110101101101001011011000110110001101001011011100110011100100000011110010110111101110101011100100010000001100010011100100110000101101001011011100010000001101100011010010110101101100101001000000110000100100000011100000110111101101001011100110110111101101110011011110111010101110011001000000110110101110101011100110110100001110010011011110110111101101101")
(define dec (list 18 18 29 45 8 6 45 41 27 6 49 41 27 38 28 32 30 22 61 53 28 34 1 34 28 38 5 41 27 34 1 44 26 22 45 37 8 6 4 32 28 6 61 41 28 54 61 46 27 55 21 51 8 6 53 53 28 54 33 50 27 54 61 45))
; Also, "I'm killing your brain like a poisonous mushroom"

(define gBinary (hexadecimal-string->binary-string hex))
(display (equal? binary gBinary))
(define gDec (binary-string->decimal-list gBinary))
(display (equal? dec gDec))
(define gBase64 (base64-encode hex))
(display (equal? base64 gBase64))

(define maryHex "4D61727920686164")
(define maryBinary "0100110101100001011100100111100100100000011010000110000101100100")
; Also, "Mary had"
(define maryDec (list 19 22 05 50 30 18 01 40 24 22 16))
(define maryBase64 "TWFyeSBoYWQ=")

(define gMaryBinary (hexadecimal-string->binary-string maryHex))
(display (equal? maryBinary gMaryBinary))
(define gMaryDec (binary-string->decimal-list maryBinary))
(display (equal? maryDec gMaryDec))
(define gMaryBase64 (base64-encode maryHex))
(display (equal? maryBase64 gMaryBase64))

