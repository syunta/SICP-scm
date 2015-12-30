(add-load-path "../../lib" :relative)
(load "amb")
(load "natural-language-parser")

(define (main args)
  (print-ambeval
    '(parse '(The professor lectures
                  to the student
                  in the class
                  with the cat)) 5)

  ; 教授が猫と一緒に教室内で生徒にレクチャーする
  '(sentence
     (simple-noun-phrase
       (article The)
       (noun professor))
     (verb-phrase
       (verb-phrase
         (verb-phrase
           (verb lectures)
           (prep-phrase
             (prep to)
             (simple-noun-phrase
               (article the)
               (noun student))))
         (prep-phrase
           (prep in)
           (simple-noun-phrase
             (article the)
             (noun class))))
       (prep-phrase
         (prep with)
         (simple-noun-phrase
           (article the)
           (noun cat)))))

  ; 教授が猫のいる教室内で生徒にレクチャーする
  '(sentence
     (simple-noun-phrase
       (article The)
       (noun professor))
     (verb-phrase
       (verb-phrase
         (verb lectures)
         (prep-phrase
           (prep to)
           (simple-noun-phrase
             (article the)
             (noun student))))
       (prep-phrase
         (prep in)
         (noun-phrase
           (simple-noun-phrase
             (article the)
             (noun class))
           (prep-phrase
             (prep with)
             (simple-noun-phrase
               (article the)
               (noun cat)))))))

  ; 教授が猫と一緒に教室内の生徒にレクチャーする
  '(sentence
     (simple-noun-phrase
       (article The)
       (noun professor))
     (verb-phrase
       (verb-phrase
         (verb lectures)
         (prep-phrase
           (prep to)
           (noun-phrase
             (simple-noun-phrase
               (article the)
               (noun student))
             (prep-phrase
               (prep in)
               (simple-noun-phrase
                 (article the)
                 (noun class))))))
       (prep-phrase
         (prep with)
         (simple-noun-phrase
           (article the)
           (noun cat)))))

  ; 教授が教室内の猫と一緒の生徒にレクチャーする
  '(sentence
     (simple-noun-phrase
       (article The)
       (noun professor))
     (verb-phrase
       (verb lectures)
       (prep-phrase
         (prep to)
         (noun-phrase
           (noun-phrase
             (simple-noun-phrase
               (article the)
               (noun student))
             (prep-phrase
               (prep in)
               (simple-noun-phrase
                 (article the)
                 (noun class))))
           (prep-phrase
             (prep with)
             (simple-noun-phrase
               (article the)
               (noun cat)))))))

  ; 教授が猫のいる教室内の生徒にレクチャーする
  '(sentence
     (simple-noun-phrase
       (article The)
       (noun professor))
     (verb-phrase
       (verb lectures)
       (prep-phrase
         (prep to)
         (noun-phrase
           (simple-noun-phrase
             (article the)
             (noun student))
           (prep-phrase
             (prep in)
             (noun-phrase
               (simple-noun-phrase
                 (article the)
                 (noun class))
               (prep-phrase
                 (prep with)
                 (simple-noun-phrase
                   (article the)
                   (noun cat)))))))))
  )
