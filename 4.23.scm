(load "./separated-analysis-excution-eval-apply")

; 本文の版は、実行時に順次に実行手続きを評価していくだけ。
; 個々の式を解析するだけでなく、並びをネストした実行手続きに変換することで実行時の並び自身の解析の手間を無くしている。

; Alyssaの版は、個々の式をは解析されるが、実行時に null? などを使い並び自身の解析を行なっている。
; これは解析時に可能な仕事なので、本文の版の方が優れている。

; 並びが1つの式しか持たない場合
'(begin 1)

; 本文の版
'(lambda (env)
   ((lambda (env) 1) env))

; Alyssaの版
'(lambda (env)
   (execute-sequence '((lambda (env) 1))
                     env))

; 並びが2つの式を持つ場合
'(begin 1 2)

; 本文の版
'(lambda (env)
   ((lambda (env) 1) env)
   ((lambda (env) 2) env))

; Alyssaの版
'(lambda (env)
   (execute-sequence '((lambda (env) 1)
                       (lambda (env) 2))
                     env))
