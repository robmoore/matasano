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

(define (bit-strings->ascii-string bs)
  (apply string (map ascii->char (map bit-string->unsigned-integer bs))))

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
  (define (make-xor-results encoded-string)
    (let ((ascii-chars (char-set-members (ascii-range->char-set 0 127)))
          (encoded-bit-strings (bit-string-split (hex-string->bit-string encoded-string) 8)))
      (define (make-result c)
        (let ((decoded-string (encoded-bit-strings->ascii-string encoded-bit-strings c)))
          (list c (common-letter-frequency decoded-string) decoded-string)))
      (map make-result ascii-chars)))
  (let ((results (make-xor-results encoded-string)))
    (if (string-null? encoded-string)
        (error "Cannot decode empty string!")
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
    contents))

(define (flatmap proc seq) 
    (fold-right append '() (map proc seq))) 
