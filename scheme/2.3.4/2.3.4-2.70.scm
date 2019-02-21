(define (log . args) (for-each display args) (newline))
(define (null-log . args) void)

(include "huffman-code.scm")

(define rock-char-pairs '(
 ("A"     2) ("NA"    16)
 ("BOOM"  1) ("SHA"    3)
 ("GET"   2) ("YIP"    9)
 ("JOB"   2) ("WAH"    1)
))

(define rock-tree (build-huffman-tree null-log rock-char-pairs))
(define rock-dict (huffman-encode-dict rock-tree))

(log "Rock pairs: " rock-char-pairs)
(log (huffman-tree->str rock-tree))

(define rock-song '(
 "GET" "A" "JOB"
 "SHA" "NA" "NA" "NA" "NA" "NA" "NA" "NA" "NA"
 "GET" "A" "JOB"
 "SHA" "NA" "NA" "NA" "NA" "NA" "NA" "NA" "NA"
 "WAH" "YIP" "YIP" "YIP" "YIP" "YIP" "YIP" "YIP" "YIP"
 "SHA" "BOOM"
))

(define rock-bits (huffman-encode-flat rock-dict rock-song))
(log "Rock song: " rock-song)
(log "Encoded as " (length rock-bits) " bits: " rock-bits)

(define rock-song-chars (huffman-accumulate rock-song 0
 (lambda (l s) (+ l (string-length s)))))

(define rock-song-spaces (- (length rock-song) 1))

(log "Song contains " rock-song-chars " characters and "
 rock-song-spaces " spaces, or " (* 8 (+ rock-song-chars
  rock-song-spaces)) " bits with fixed 8-bit coding")
