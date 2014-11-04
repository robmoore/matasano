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

(define hd-example-1 "this is a test")
(define hd-example-2 "wokka wokka!!!")

; Read in 6.txt as base64 and create bit-strings
; Need to write decode-base64 function 

; Determine key size

; Break into key size blocks

; Create composite blocks from the 1st byte of each block, 2nd byte of each block

; Solve for each block as if it were encoded with single-byte XOR cipher

; For each block find the best look histogram for each key to determine which is the key

(define (hamming-distance a b)
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
  ; create a list of the xor-d results for each byte
  (define (xor-list a-bss b-bss)
    (define (xor a-bss b-bss acc)
      (if (null? a-bss)
          acc
          (xor (cdr a-bss) (cdr b-bss) (cons (bit-string-xor (car a-bss) (car b-bss)) acc))))
    (reverse (xor a-bss b-bss '())))
  (let ((a-bss (ascii-string->bit-strings a))
        (b-bss (ascii-string->bit-strings b)))
    (let ((xor-bss (xor-list a-bss b-bss)))
      (fold-right + 0 (map count-ones xor-bss)))))d

; test hamming distance
(= (hamming-distance hd-example-1 hd-example-2) 37)