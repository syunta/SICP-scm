(add-load-path "../../lib" :relative)
(load "table")

; a
;
; ファイルに対して追加されるべき型情報は、社員情報の集合の構造
; list, ordered-list, binary-tree, etc ...
;
; レコードに対して追加されるべき型情報は、レコードの集合の構造
; (name salary address), (address name salary), etc ... 
;
; (key value)の構造も事業所毎に異なるので、
; これも追加されるべき型情報である
; (key value), (value . key), (value), etc ...
;
; これ以上の型情報の追加は不毛なので、ここまでにしておく
;
; 追加される型情報が複数になったので、タグをリストで扱う
;
; 通常、 name はアルファベットで表現されるが
; この順序付けを実装していると問題の本筋から外れてしまうので
; 今回は name を数値で表現することにする

; b
;
; この演算が働くためには、引数として受け取るレコードが、
; レコードの集合の型情報を持つよう構造化されている必要がある
;
; よって、a で実装した get-record は record をそのまま返していたが、
; これに、型情報を付加した上で record を返す修正が必要となる
;
; 実装の統一感を出すため、get-name , get-value の引数に対しても、型情報を含める形に変更を加えた

; c
;
; name に重複があった場合は、リストにまとめて返すこととした

; d
;
; これまでに実装してきた (install-***-package) のように、
; 合併した事業所のデータ構造にあわせて package を追加してあげればよい
; 事業所を示すタグ名が被らないことに注意する

(define (get-record file name)
  (let ((tags (type-tag file)))
    ((get 'get-record (file-type tags)) name file)))

(define (find-employee-record files name)
  (if (null? files)
    '()
    (let ((record (get-record (car files) name)))
      (if (null? record)
        (find-employee-record (cdr files) name)
        (cons (contents record)
              (find-employee-record (cdr files) name))))))

(define (get-name record)
  (let ((tags (type-tag record)))
    ((get 'get-name (record-type tags)) record)))

(define (get-salary record)
  (let ((tags (type-tag record)))
    ((get 'get-salary (record-type tags)) record)))

(define (get-value set-of-key-value)
  (let ((tags (type-tag set-of-key-value)))
    ((get 'get-value (key-value-type tags)) set-of-key-value)))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad tagged datum -- TYPE=TAG" datum)))

(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad tagged datum -- CONTENTS" datum)))

(define (file-type tags) (car tags))
(define (record-type tags) (cadr tags))
(define (key-value-type tags) (caddr tags))

(define (install-unordered-list-package)
  (define (get-record given-key file)
    (define tags (type-tag file))
    (define (lookup set-of-records)
      (cond ((null? set-of-records) '())
            ((= given-key (get-name (attach-tag tags (car set-of-records))))
             (car set-of-records))
            (else (lookup (cdr set-of-records)))))
    (attach-tag tags (lookup (contents file))))
  (put 'get-record 'unordered-list get-record)
  'done)

(define (install-ordered-list-package)
  (define (get-record given-key file)
    (define tags (type-tag file))
    (define (lookup set-of-records)
      (if (null? set-of-records)
        '()
        (let ((name (get-name (attach-tag tags (car set-of-records)))))
          (cond ((< given-key name) '())
                ((= given-key name) (car set-of-records))
                (else (lookup (cdr set-of-records)))))))
    (attach-tag tags (lookup (contents file))))
  (put 'get-record 'ordered-list get-record)
  'done)

(define (install-jp-record-package)
  (define (get-name tag-record)
    (let ((tags (type-tag tag-record))
          (record (contents tag-record)))
      (let ((tagged-key-value (attach-tag tags (car record))))
        (get-value tagged-key-value))))
  (define (get-salary tag-record)
    (let ((tags (type-tag tag-record))
          (record (contents tag-record)))
      (let ((tagged-key-value (attach-tag tags (cadr record))))
        (get-value tagged-key-value))))
  (put 'get-name 'jp get-name)
  (put 'get-salary 'jp get-salary)
  'done)

(define (install-us-record-package)
  (define (get-name tag-record)
    (let ((tags (type-tag tag-record))
          (record (contents tag-record)))
      (let ((tagged-key-value (attach-tag tags (cadr record))))
        (get-value tagged-key-value))))
  (define (get-salary tag-record)
    (let ((tags (type-tag tag-record))
          (record (contents tag-record)))
      (let ((tagged-key-value (attach-tag tags (caar record))))
        (get-value tagged-key-value))))
  (put 'get-name 'us get-name)
  (put 'get-salary 'us get-salary)
  'done)

(define (install-dot-list-key-value-package)
  (define (get-value tagged-key-value)
    (cdr (contents tagged-key-value)))
  (put 'get-value 'dot-list get-value)
  'done)

(define (install-list-key-value-package)
  (define (get-value tagged-key-value)
    (cadr (contents tagged-key-value)))
  (put 'get-value 'list get-value)
  'done)

(define jp-file (attach-tag '(ordered-list jp list)
                            '(((name 2) (salary 1000) (address Tokyo  ))
                              ((name 6) (salary 2000) (address Nagano ))
                              ((name 8) (salary 7000) (address Tottori)))))

(define us-file (attach-tag '(unordered-list us dot-list)
                            '((((salary . 10) (address . L.A)) (name . 5))
                              (((sarary . 15) (address . NewYork)) (name . 2))
                              (((salary . 40) (address . Boston)) (name . 7)))))

(define files (list jp-file us-file))

(define (main args)
  (install-unordered-list-package)
  (install-ordered-list-package)
  (install-jp-record-package)
  (install-us-record-package)
  (install-dot-list-key-value-package)
  (install-list-key-value-package)
  (print (get-salary (get-record jp-file 6)))
  (print (get-salary (get-record us-file 7)))
  (print (find-employee-record files 2))
  )
