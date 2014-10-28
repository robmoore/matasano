; http://cryptopals.com/sets/1/challenges/2/
; Fixed XOR

; Write a function that takes two equal-length buffers and produces their XOR combination.
; If your function works properly, then when you feed it the string:
;  	1c0111001f010100061a024b53535009181c
; ... after hex decoding, and when XOR'd against:
;	686974207468652062756c6c277320657965
; ... should produce:
;	746865206b696420646f6e277420706c6179

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

(define (safe-bit-string-head s end) ; gives 'up to' 6 or remaining
  (let ((sl (bit-string-length s)))
    (if (>= sl end)
        (bit-substring s 0 end)
        (bit-substring s 0 sl)))) 

(define (safe-bit-string-tail s start) ; gives 'up to' 6 and remaining
  (let ((sl (bit-string-length s)))
    (if (>= sl start)
        (bit-substring s start sl)
        (make-bit-string 0 #f))))

(define (bit-string-split bs split-at)
  (define (split bss ac)
    (if (= (bit-string-length bss) 0)
        ac
        (split (safe-bit-string-tail bss split-at) (cons (safe-bit-string-head bss split-at) ac))))
  (split bs '())) ; bit-substring works in reverse (0 = end) so these results will be in insertion order

(define (hex-string->bit-string hs)
  (let ((hs-lst (string-split hs 2))
        (p (lambda (x) (unsigned-integer->bit-string 8 (string->number x 16)))))
    (fold-right (lambda (x r) (bit-string-append (p x) r)) (make-bit-string 0 #f) hs-lst)))

(define (bit-string->hex-string bs)
  (let ((bs-lst (bit-string-split bs 8))
        (p (lambda (x) (string-pad-left (number->string (bit-string->unsigned-integer x) 16) 2 #\0))))
    (fold-right (lambda (x r) (string-append r (p x))) "" bs-lst)))

(define hex1 "1c0111001f010100061a024b53535009181c")
(define hex2 "686974207468652062756c6c277320657965")
(define expected "746865206b696420646f6e277420706c6179")

(define xor-out (bit-string->hex-string (bit-string-xor (hex-string->bit-string hex1) (hex-string->bit-string hex2))))
(equal? xor-out expected)