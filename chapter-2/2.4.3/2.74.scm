(add-load-path "../.." :relative)
(load "lib/library.scm")

; a
;
; ファイルに対して追加されるべき型情報は、社員情報の集合の構造
; list, ordered-list, binary-tree, etc ...

; レコードに対して追加されるべき型情報は、レコードの集合の構造
; (name salary address), (address name salary), etc ... 

; (key value)の構造も事業所毎に異なるので、
; これも追加されるべき型情報である
; (key value), (value . key), (value), etc ...

; これ以上の型情報の追加は不毛なので、ここまでにしておく

; 追加される型情報が複数になったので、タグをリストで扱う

; 通常、 name はアルファベットで表現されるが
; この順序付けを実装していると問題の本筋から外れてしまうので
; 今回は name を数値で表現することにする

(define (get-record personnel-file name)
  (let ((tags (type-tag personnel-file)))
    ((get 'get-record (file-type tags)) name
                                        tags
                                        (contents personnel-file))))
(define (get-name tags record)
  ((get 'get-name (record-type tags)) tags record))

(define (get-value tags set-of-key-value)
  ((get 'get-value (key-value-type tags)) set-of-key-value))

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
  (define (lookup given-key tags set-of-records)
    (if (null? set-of-records)
      (error "Not found")
      (let ((record (car set-of-records)))
        (if (= given-key (get-name tags record))
          record
          (lookup given-key tags (cdr set-of-records))))))
  (put 'get-record 'unordered-list lookup)
  'done)

(define (install-ordered-list-package)
  (define (lookup given-key tags set-of-records)
    (if (null? set-of-records)
      (error "Not found")
      (let ((record (car set-of-records)))
        (let ((name (get-name tags record)))
          (cond ((< given-key name) (error "Not found"))
                ((= given-key name) record)
                (else (lookup given-key tags (cdr set-of-records))))))))
  (put 'get-record 'ordered-list lookup)
  'done)

(define (install-jp-record-package)
  (define (get-name tags record)
    (let ((name-value (car record)))
      (get-value tags name-value)))
  (put 'get-name 'jp get-name)
  'done)

(define (install-us-record-package)
  (define (get-name tags record)
    (let ((name-value (cadr record)))
      (get-value tags name-value)))
  (put 'get-name 'us get-name)
  'done)

(define (install-plain-key-value-package)
  (define (get-value set-of-key-value) set-of-key-value)
  (put 'get-value 'plain get-value)
  'done)

(define (install-standard-key-value-package)
  (define (get-value set-of-key-value) (cadr set-of-key-value))
  (put 'get-value 'standard get-value)
  'done)

(define jp-file '(((name 2) (salary 1000) (address Tokyo  ))
                  ((name 6) (salary 2000) (address Nagano ))
                  ((name 8) (salary 7000) (address Tottori))))

(define us-file '(((10 L.A    ) 5)
                  ((15 NewYork) 2)
                  ((40 Boston ) 7)))

(define (main args)
  (install-unordered-list-package)
  (install-ordered-list-package)
  (install-jp-record-package)
  (install-us-record-package)
  (install-plain-key-value-package)
  (install-standard-key-value-package)
  (print (get-record (attach-tag '(ordered-list jp standard) jp-file) '6))
  (print (get-record (attach-tag '(unordered-list us plain) us-file) '7)))
