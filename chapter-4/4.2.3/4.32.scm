(add-load-path "../../lib" :relative)
(load "stream")
(load "stream-lazy-list")

(define count 0)

(define (id x)
  (set! count (+ count 1))
  x)

(define stream (cons-stream (id 11) (cons-stream (id 22) (cons-stream (id 33) the-empty-stream))))

(for-each (lambda (exp) (actual-value exp the-global-environment))
          '((define count 0)
            (define (id x)
              (set! count (+ count 1))
              x)
            (define stream-lazy-list (cons (id 11) (cons (id 22) (cons (id 33) '()))))))

(for-each (lambda (exp) (actual-value exp the-global-environment))
          '((define (make-tree seq)
              (cons (make-tree (cdr seq)) ;left-branch
                    (cons (car seq) ;entry
                          (make-tree (cdr seq))))) ;right-branch
            (define (tree-take tree depth)
              (if (= depth 0)
                '()
                (let ((left-branch (car (cdr tree)))
                      (entry (car tree))
                      (right-branch (cdr (cdr tree))))
                  (cons (tree-take left-branch (- depth 1))
                        (cons entry
                              (tree-take right-branch (- depth 1)))))))))

(define (main args)
  ; streamではstream-refするだけでもcons-streamのcar部分の手続きが順次に実行される。
  ; 遅延度の高いリストでは、list-refの対象のcar部分だけが実行される。
  (stream-ref stream 2)
  (print count)
  ;=> 3
  (actual-value '(list-ref stream-lazy-list 2) the-global-environment)
  (print (actual-value 'count the-global-environment))
  ;=> 1

  ; 余分な遅延度は無限ツリーに利用できる。
  ; ツリーのノードを (left-branch entry right-branch) という構造にすると、streamでは無限ループになる。
  (actual-value '(car (tree-take (make-tree integers) 3)) the-global-environment)
  )
