(load "./2.70")

(define n=5
  '((A 1) (B 2) (C 4) (D 8) (E 16)))
;
;                {ABCDE} 31
;               /       \
;            {ABCD} 15  E 16
;           /      \
;        {ABC} 7   D 8
;       /     \
;     {AB} 3  C 4
;    /    \
;  A 1    B 2

(define n=10
  '((A 1) (B 2) (C 4) (D 8) (E 16) (F 32) (G 64) (H 128) (I 256) (J 512)))
;
;                                                 {ABCDEFGHIJ} 1023
;                                                /            \
;                                          {ABCDEFGHI} 511   J 512
;                                         /           \
;                                   {ABCDEFGH} 255   I 256
;                                  /          \
;                            {ABCDEFG} 127   H 128
;                           /         \
;                      {ABCDEF} 63    G 64
;                     /        \
;                 {ABCDE} 31   F 32
;                /       \
;            {ABCD} 15   E 16
;           /      \
;        {ABC} 7   D 8
;       /     \
;    {AB} 3   C 4
;   /    \
; A 1    B 2

; 最高頻度の記号は 1bit
; 最低頻度の記号は n - 1 bit

(define (main args)
  (print (encode '(E) (generate-huffman-tree n=5))) ;最高頻度
  ;=> (1)
  (print (encode '(A) (generate-huffman-tree n=5))) ;最低頻度
  ;=> (0 0 0 0)

  (print (encode '(J) (generate-huffman-tree n=10))) ;最高頻度
  ;=> (1)
  (print (encode '(A) (generate-huffman-tree n=10))) ;最低頻度
  ;=> (0 0 0 0 0 0 0 0 0)
  )
