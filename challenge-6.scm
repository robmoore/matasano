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

(define byte-length 8)

; Assumes we are processing a line at a time. Returns a list of 8-bit bit-strings.
(define (base64->bit-strings b)
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
      (let ((trim-bs-lst (lambda (bs) (bit-substring bs (* diff 2) (bit-string-length bs)))))
        ; Need to chop off 0, 2, or 4 zeros on end based on number of equals in incoming string
        (let ((trimmed-bs-lst (append (except-last-pair bs-lst) (map trim-bs-lst (last-pair bs-lst)))))
          ; Need to reverse bs-lst so we end up with bit-string built from front to back for ordering's sake.
          ; Otherwise, we end up with the last two bits of the next item at the front of the first bit-string rather than  
          ; what we want which is to add the next two bits from the following value to the end so get the
          ; correct value when we substring the bit-string later.
          ;   reversed: 010011 010101 -> 01001101 (right)
          ; unreversed: 010110 010011 -> 10010011 (wrong)
          (reverse (bit-string-split (fold-right bit-string-append #* (reverse trimmed-bs-lst)) 8)))))))

(define base64 "TWFyeSBoYWQ=")
(define base64-bss (base64->bit-strings base64))
(equal? (bit-strings->ascii-string base64-bss) "Mary had")

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

(define hd-example-1 "this is a test")
(define hd-example-2 "wokka wokka!!!")
(= (bit-string-hamming-distance (ascii-string->bit-string hd-example-1) (ascii-string->bit-string hd-example-2)) 37)

(define enc (read-file "6.txt"))
; join together all bit-strings
(define enc-bss (flatmap base64->bit-strings enc))
; test that contents is in the right order
(equal? (take enc-bss 45) (base64->bit-strings (car enc)))

(define enc-bs (fold-right bit-string-append #* enc-bss))

(define (calc-keysize-hamming-distances bs)
  (define (make-offset-pairs ranges acc)
    (if (null? (cdr ranges)) ; if we don't have another end value, stop
        acc
        (make-offset-pairs (cdr ranges) (cons (list (car ranges) (cadr ranges)) acc))))
  (define (split-bit-string offsets)
    (bit-substring bs (car offsets) (cadr offsets)))
  (define (calc-distance size)
    (define (make-keys size number)
      (let ((offsets (make-offset-pairs (iota (+ number 1) 0 size) '())))
        (map split-bit-string offsets)))
    (define (normalized-hamming-distance key-pair)
      (/ (bit-string-hamming-distance (car key-pair) (cadr key-pair)) (bit-string-length (car key-pair))))
    (define (avg elements)
      (/ (fold-right + 0 elements) (length elements)))
    (let ((bytes (* size byte-length)))
      (let ((key-combos (combine (make-keys bytes 4)))) ; test out first 4 chunks for this key-size
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

(define (derive-key-size enc-bs)
  (car (first (calc-keysize-hamming-distances enc-bs))))

; Derive key size using lowest scoring hamming-distance
(define candidate-key-size (derive-key-size enc-bs))

; Create composite blocks from the 1st byte of each block, 2nd byte of each block
(define (make-group-blocks bs group-count)
  (let ((bss (bit-string-split bs byte-length)))
    (let ((bss-length (length bss)))
      (define (make-group-block group-number)
        (let ((ks (filter (lambda (x) (< x bss-length)) (iota (integer-ceiling bss-length group-count) (- group-number 1) group-count))))
          (map (lambda (k) (list-ref bss k)) ks)))
      (map make-group-block (iota group-count 1)))))

; Break into key size blocks
; Solve for each block as if it were encoded with single-byte XOR cipher
(define (derive-key enc-bs key-size)
  (define (group-blocks->keys group-blocks)
    (map decode-bit-strings-without-key group-blocks))
  (let ((group-blocks (make-group-blocks enc-bs key-size)))
    (list->string (map (lambda (item) (car item)) (group-blocks->keys group-blocks)))))

; Create a key from each 'best' individual key and decode message
(define candidate-key (derive-key enc-bs candidate-key-size))
(decode-with-repeating-key enc-bss (map char->bit-string (string->list candidate-key)))

