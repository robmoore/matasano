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
        #*)))

(define (bit-string-split bs split-at)
  (define (split bss ac)
    (if (= (bit-string-length bss) 0)
        ac
        (split (safe-bit-string-tail bss split-at) (cons (safe-bit-string-head bss split-at) ac))))
  (reverse (split bs '())))

(define (hex-string->bit-string hs)
  (let ((hs-lst (string-split hs 2))
        (p (lambda (x) (unsigned-integer->bit-string 8 (string->number x 16)))))
    (fold-right (lambda (x r) (bit-string-append (p x) r)) (make-bit-string 0 #f) hs-lst)))

(define (hex-string->bit-strings hs)
  (let ((bs (hex-string->bit-string hs)))
    (bit-string-split bs 8)))

(define (bit-string->hex-string bs)
  (let ((bs-lst (bit-string-split bs 8)))
    (bit-strings->hex-string bs-lst)))

(define (bit-strings->hex-string bs-lst)
  (let ((p (lambda (x) (string-pad-left (number->string (bit-string->unsigned-integer x) 16) 2 #\0))))
    (fold-right (lambda (x r) (string-append (p x) r)) "" bs-lst)))

(define (bit-strings->ascii-string bs-lst)
  (apply string (map ascii->char (map bit-string->unsigned-integer bs-lst))))

(define (ascii-string->bit-string s)
  (fold-right bit-string-append #* (ascii-string->bit-strings s)))

(define (ascii-string->bit-strings s)
  (let ((ascii-chars (map char->ascii (string->list s))))
    (map (lambda (c) (unsigned-integer->bit-string 8 c)) ascii-chars)))

(define (encoded-bit-strings->ascii-string bit-strings key-char)
  (let ((decoded-bit-strings (apply-xor-with-char bit-strings key-char)))
    (bit-strings->ascii-string decoded-bit-strings)))

(define (common-letter-frequency s)
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

(define (apply-xor bit-strings key-bit-string)
  (map (lambda (bs) (bit-string-xor bs key-bit-string)) bit-strings))

(define (apply-xor-with-char bit-strings key-char)
  (let ((key-bit-string (unsigned-integer->bit-string 8 (char->ascii key-char))))
    (apply-xor bit-strings key-bit-string)))

(define (encode string key-char)
  (let ((bit-strings (ascii-string->bit-strings string)))
    (define xor-results (apply-xor-with-char bit-strings key-char))
    (bit-strings->hex-string xor-results)))

(define (decode-with-key encoded-string key-char)
  ; convert to bit-strings from hex
  (let ((bit-strings (bit-string-split (hex-string->bit-string encoded-string) 8)))
    (define xor-results (apply-xor-with-char bit-strings key-char))
    (bit-strings->ascii-string xor-results)))

(define (make-xor-results-frequency-comparator x y)
  (let ((x-score (second x)) (y-score (second y)))
    (>= x-score y-score)))

; Returns a list with the most likely decoding based on ASCII characters (0 to 127) 
; as the key in the format: (key, frequency score, decoded text)
(define (decode-without-key encoded-string)
      (let ((encoded-bit-strings (bit-string-split (hex-string->bit-string encoded-string) 8)))
        (decode-bit-strings-without-key encoded-bit-strings)))

(define (decode-bit-strings-without-key encoded-bit-strings)
  (define (make-xor-results encoded-bit-strings)
    (let ((ascii-chars (char-set-members (ascii-range->char-set 0 127))))
      (define (make-result c)
        (let ((decoded-string (encoded-bit-strings->ascii-string encoded-bit-strings c)))
          (list c (common-letter-frequency decoded-string) decoded-string)))
      (map make-result ascii-chars)))
  (let ((results (make-xor-results encoded-bit-strings)))
    (if (null? encoded-bit-strings)
        (error "Cannot decode empty bit-string list!")
        ; Return the most likely result
        (first (sort results make-xor-results-frequency-comparator)))))

(define (read-file file-name)
  (define (file->list in-port ac)
    (let ((line (read-line in-port)))
      (if (eof-object? line)
          ac
          (file->list in-port (cons line ac)))))
  (let ((in-port (open-input-file file-name)))
    (define contents (file->list in-port '()))
    (close-input-port in-port)
    (reverse contents)))

(define (flatmap proc seq) 
    (fold-right append '() (map proc seq))) 

; combines list contents into unique pairs -- (list "A", "B", "C", "D") -> (("A" "B") ("A" "C") ("A" "D") ("B" "C") ("B" "D") ("C" "D"))
(define (combine lst)
  (define (pair it li)
    (map (lambda (x) (list it x)) li))
  (flatmap (lambda (k) (pair (list-ref lst k) (list-tail lst (+ k 1)))) (iota (length lst))))

(define (char->bit-string char)
  (unsigned-integer->bit-string 8 (char->ascii char)))

; Returns encoded value in hex
(define (encode-with-repeating-key enc-string key-chars)
  (let ((enc-bss (ascii-string->bit-strings enc-string))
        (key-bss (map char->bit-string key-chars))))
    (encode-with-repeating-key enc-bss key-bss))

(define (encode-with-repeating-key enc-bss key-bss)
  (define (enc bs ks acc)
    (if (null? bs)
        acc
        (enc (cdr bs) (cdr ks) (cons (bit-string-xor (car bs) (car ks)) acc))))
  (let ((circ-key-bss (apply circular-list key-bss)))
    (bit-strings->hex-string (reverse (enc enc-bss circ-key-bss '())))))

; Assumes enc-string is in hex
(define (decode-with-repeating-key enc-string key-chars)
  (let ((enc-bss (hex-string->bit-strings enc-string))
        (key-bss (map char->bit-string key-chars)))
    (decode-with-repeating-key enc-bss key-bss)))

(define (decode-with-repeating-key enc-bss key-bss)
  (define (dec bs ks acc)
    (if (null? bs)
        acc
        (dec (cdr bs) (cdr ks) (cons (bit-string-xor (car bs) (car ks)) acc))))
  (let ((circ-key-bss (apply circular-list key-bss)))
    (bit-strings->ascii-string (reverse (dec enc-bss circ-key-bss '())))))
