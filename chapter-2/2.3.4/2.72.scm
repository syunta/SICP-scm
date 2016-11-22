(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (encode-symbol s tree)
  (define (iter tree bits)
    (cond ((leaf? tree)
           (reverse bits))
          ((memq s (symbols (left-branch tree)))
           (iter (left-branch tree) (cons 0 bits)))
          ((memq s (symbols (right-branch tree)))
           (iter (right-branch tree) (cons 1 bits)))
          (else
            (error "ENCODE-SYMBOL -- Symbol Not Found " s))))
  (iter tree '()))

(define n=5
  '((A 1) (B 2) (C 4) (D 8) (E 16)))
; n = 5
;                {ABCDE} 31
;               /       \
;            {ABCD} 15  E 16
;           /      \
;        {ABC} 7   D 8
;       /     \
;     {AB} 3  C 4
;    /    \
;  A 1    B 2

; 上記のような encode-symbol の実装だと、最高頻度の記号を符号化する場合、
; 5 + 1 -> n + 1 となる。
;
; このようなハフマン木で符号化する場合、右の枝から調べる方が効率が良い。
; その場合、 1 となる。

; 上記のような encode-symbol の実装だと、最低頻度の記号を符号化する場合、
; 最低頻度の記号は集合の先頭にあるので、
; 1 + 1 + 1 + 1 + 1 -> n となる。
;
; このようなハフマン木で符号化する場合、左の枝から調べる方が効率が良い。
; もし右の枝から調べる場合、 2n となる。

; 右か左のどちらの枝を優先して探索するかで効率が変わるが、
; 基本的には最高頻度の記号に最も効率良く辿り着けるように設計すると良いと考えられる。
