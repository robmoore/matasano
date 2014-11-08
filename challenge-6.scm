; Break repeating-key XOR
;
; There's a file here (6.txt). It's been base64'd after being encrypted with repeating-key XOR.
; 
; Decrypt it.
;
; Here's how:
;
; 1. Let KEYSIZE be the guessed length of the key; try values from 2 to (say) 40.
; 2. Write a function to compute the edit distance/Hamming distance between two strings. The 
;    Hamming distance is just the number of differing bits. The distance between:
;    'this is a test' and 'wokka wokka!!!' is 37. Make sure your code agrees before you proceed.
; 3. For each KEYSIZE, take the first KEYSIZE worth of bytes, and the second KEYSIZE worth of 
;    bytes, and find the edit distance between them. Normalize this result by dividing by KEYSIZE.
; 4. The KEYSIZE with the smallest normalized edit distance is probably the key. You could proceed 
;    perhaps with the smallest 2-3 KEYSIZE values. Or take 4 KEYSIZE blocks instead of 2 and average the distances.
; 5. Now that you probably know the KEYSIZE: break the ciphertext into blocks of KEYSIZE length.
; 6. Now transpose the blocks: make a block that is the first byte of every block, and a block that 
;    is the second byte of every block, and so on.
; 7. Solve each block as if it was single-character XOR. You already have code to do this.
; 8. For each block, the single-byte XOR key that produces the best looking histogram is the repeating-key 
;    XOR key byte for that block. Put them together and you have the key.
;
; This code is going to turn out to be surprisingly useful later on. Breaking repeating-key XOR ("Vigenere") 
; statistically is obviously an academic exercise, a "Crypto 101" thing. But more people "know how" to break 
; it than can actually break it, and a similar technique breaks ;something much more important.
;
; No, that's not a mistake.
; We get more tech support questions for this challenge than any of the other ones. We promise, there aren't any 
; blatant errors in this text. In particular: the "wokka wokka!!!" edit distance really is 37.

(load "common.scm")

(define hd-example-1 "this is a test")
(define hd-example-2 "wokka wokka!!!")

; assume we are processing a line at a time
(define (base64->bit-string b)
  ; use let here instead of defines?
  (define base64-table "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
  (define char-set:base64 (string->char-set base64-table))
  (define (from-base64 b)
    (let ((offset (string-find-next-char base64-table b)))
      (if (false? offset)
          offset
          (unsigned-integer->bit-string 6 offset))))
  (let ((trim-b (string-trim-right b char-set:base64)))
    (let ((diff (- (string-length b) (string-length trim-b)))
          (bs-lst (map from-base64 (string->list trim-b))))
      ; Need to chop off 0, 2, or 4 zeros on end based on number of equals in incoming string
      (fold-right bit-string-append #* (list-head bs-lst (- (length bs-lst) (* diff 2)))))))

; bit-string is defined in Scheme so using an alternate name here
(define (bit-string-hamming-distance a-bs b-bs)
  ; map bit-string to ones count
  (define (count-ones bs)
    (let ((end (bit-string-length bs)))
      (let loop ((start 0)
                 (sum 0))
        (let ((next
                (bit-substring-find-next-set-bit bs start end)))
          (if (and next (< next end)) ; not false and smaller than end
              (loop (+ next 1) (+ sum 1))
              sum)))))
  (count-ones (bit-string-xor a-bs b-bs)))

(define enc (read-file "6.txt"))
; join together all bit-strings
(define enc-bs (fold-right bit-string-append #* (map base64->bit-string enc)))

(define (calc-keysize-hamming-distances enc-bs)
  (define (calc-distance size)
    (define (make-keys size number)
      (define (make-offset-pairs ranges acc)
        (if (null? (cdr ranges)) ; if we don't have another end value, stop
            acc
            (make-offset-pairs (cdr ranges) (cons (list (car ranges) (cadr ranges)) acc))))
      (define (make-key offsets)
        (bit-substring enc-bs (car offsets) (cadr offsets)))
      (let ((offsets (make-offset-pairs (iota (+ number 1) 0 size) '())))
        (map make-key offsets)))
    (define (normalized-hamming-distance key-pair)
      (/ (bit-string-hamming-distance (car key-pair) (cadr key-pair)) (bit-string-length (car key-pair))))
    (define (avg elements)
      (/ (fold-right + 0 elements) (length elements)))
    (let ((bytes (* size 8)))
      (let ((key-combos (combine (make-keys bytes 4))))
        (let ((normalized-distances (map normalized-hamming-distance key-combos)))
          (avg normalized-distances)))))
  (define (make-hamming-distance-results-comparator x y)
    (let ((x-dist (second x)) (y-dist (second y)))
      (<= x-dist y-dist)))
  (define (make-hamming-distance-entry size)
    (let ((dist (calc-distance size)))
      (list size dist)))
  (let ((results (map make-hamming-distance-entry (iota 39 2)))) ; sizes 2-40
    (sort results make-hamming-distance-results-comparator)))

; combines list contents into unique pairs -- (list "A", "B", "C", "D") -> (("A" "B") ("A" "C") ("A" "D") ("B" "C") ("B" "D") ("C" "D"))
(define (combine lst)
  (define (combine it li)
    (map (lambda (x) (list it x)) li))
  (flatmap (lambda (k) (combine (list-ref lst k) (list-tail lst (+ k 1)))) (iota (length lst))))

(define (derive-key-size enc-bs)
  (car (first (calc-keysize-hamming-distances enc-bs))))

; Break into key size blocks
(define (make-enc-bss enc-bs size)
  (bit-string-split enc-bs size))

(define candidate-key-size (derive-key-size enc-bs))
(define enc-bss (bit-string-split enc-bs candidate-key-size))

; Create composite blocks from the 1st byte of each block, 2nd byte of each block

; use iota to create substring list 
; map pairs to bit-string using bit-sub-string
; fold-right to build bit-string using bit-string-append
; maybe create list of lists to return this?

; Solve for each block as if it were encoded with single-byte XOR cipher

; For each block find the best look histogram for each key to determine which is the key
; common-letter-frequency ETAOIN SHRDLU

; Create a key from each 'best' individual key and decode message

; test hamming distance
(= (bit-string-hamming-distance (ascii-string->bit-string hd-example-1) (ascii-string->bit-string hd-example-2)) 37)