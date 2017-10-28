; a1 <- bq + aq + ap
; b1 <- bp + aq
; 
; a2 <- b1q + a1q + a1p ... 関係式1
; b2 <- b1p + a1q
; 
; a2 <- bq1 + aq1 + ap1 ... 関係式2
; b2 <- bp1 + aq1
; 
; a2 <- b1q + a1q + a1p ... 関係式1 を変形
; a2 <- q(bp + aq) + q(bq + aq + ap) + p(bq + aq + ap)
; a2 <- qpb + qqa + qqb + qqa + qpa + pbq + paq + ppa
; a2 <- 2qqa + qqb + 2qpa + 2qpb + ppa
; a2 <- a(2qq + 2pq + pp) + b(qq + 2qp)
; 
; a2 <- bq1 + aq1 + ap1 ... 関係式2 を変形
; a2 <- a(p1 + q1) + b(q1)
; 
; a2 <- a(2qq + 2pq + pp) + b(qq + 2qp) ... 変形した関係式同士から q1 の関係を導出
; a2 <- a(p1 + q1) + b(q1)
; 
; q1 = qq + 2qp
; 
; b2 <- b1p + a1q ... 関係式1 を変形
; b2 <- p(bp + aq) + q(bq + aq + ap)
; b2 <- ppb + pqa + qqb + qqa + qpa
; b2 <- ppb + 2pqa + qqb + qqa
; b2 <- b(pp + qq) + a(2pq + qq)
; 
; b2 <- b(pp + qq) + a(2pq + qq) ... 変形した関係式と関係式2から p1,q1 の関係を導出
; b2 <- b(p1) + a(q1)
; 
; p1 = pp + qq
; q1 = 2pq + qq

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q)) ; p'を計算
                   (+ (* 2 p q) (square q)) ; q'を計算
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(define (main args)
  (print (fib 1))
  (print (fib 2))
  (print (fib 3))
  (print (fib 4))
  (print (fib 5))
  (print (fib 6))
  (print (fib 7))
  (print (fib 8))
  (print (fib 9))
  (print (fib 10)))
