
;logical procession of query
(defun verarbeite-query (selection-list text)
  (let ((entry-list) (entry) (type) (content) (result-list '()) (keyword-result) (tmp))
    (cond  ((> (length selection-list) 0) ;only react if user selected something
            (mapcar (lambda (keyword) ;iterate over keywords
                      (print keyword)
                      (setq entry-list (gethash keyword *keyword-dict*))
                      (setq keyword-result '()) ;contains all documents in which the current keyword contains specified text
                      (mapcar (lambda (entry)  ;iterate over all entries for current keyword
                                (setq docID (keyword-entry-doc-id entry))
                                (setq type (keyword-entry-keyword-type entry))
                                (setq content (keyword-entry-content entry))
                                (cond ((and (equal type 'cons) (recursive-list-search text content)) ;different search procedures according to type
                                       (setq keyword-result (cons docID keyword-result)))
                                      ((and (equal type 'simple-base-string) (string-search text content)) ;union is necessary if keyword occurs multiple times in document (e.g. different cases)
                                       (setq keyword-result (union (list docID) keyword-result))) 
                                      ((and (equal type 'fixnum) (num-search text content))
                                       (setq keyword-result (union (list docID) keyword-result)))))
                              entry-list)
                      (setq result-list (cons keyword-result result-list)))
                    selection-list)

            
                   (if (equal (capi:choice-selected-item radio-buttons) button-or) 
                       (setq result-list (connect-or result-list)) ;connect inner term with and/or
                     (setq result-list (setq result-list (connect-and result-list))))


            (cond ((not (null (member 'NOT (capi:choice-selected-items not-operator-panel))))  ;if selected, surround with not
                   (setq result-list (negate result-list)))))) result-list)) 


(defun recursive-list-search (text content)
  (let ((elem) (found nil))
    (cond ((null (car content)) nil)
           (t (setq elem (car content)) ;erstes element durchsuchen
              (cond ((equal (type-of elem) 'simple-base-string) (setq found (string-search text elem)))
                    ((equal (type-of elem) 'fixnum) (setq found (num-search text elem)))
                    ((equal (type-of elem) 'cons) (setq found (recursive-list-search text elem)))) ;sublisten auch durchsuchen
              (if found t (recursive-list-search text (cdr content)))))))
          
              


(defun string-search (text content)
  (search text content))




(defun num-search (text content)
  (let ((num (ignore-errors (read-from-string text))))
    (cond ((numberp num) (= num content))
          (t (cond ((equalp text "null") (setq num 0))
                   ((equalp text "eins") (setq num 1))
                   ((equalp text "zwei") (setq num 2))
                   ((equalp text "drei") (setq num 3))
                   ((equalp text "vier") (setq num 4))
                   ((equalp text "fünf") (setq num 5))
                   ((equalp text "sechs") (setq num 6))
                   ((equalp text "sieben") (setq num 7))
                   ((equalp text "acht") (setq num 8))
                   ((equalp text "neun") (setq num 9))
                   ((equalp text "zehn") (setq num 10))
                   ((equalp text "elf") (setq num 11))
                   ((equalp text "zwölf") (setq num 12)))
             (if (numberp num) (= num content))))))




(defun connect-and (result-list)
 (let ((and-res))
  (setq result-list (sort result-list '> :key 'length))
  (setq and-res (car result-list))
  (setq result-list (cdr result-list))
  (mapcar (lambda (x) 
            (setq and-res (intersection and-res x))) result-list)
  and-res))
    


(defun connect-or (result-list)
  (let ((or-res))
    (setq or-res (car result-list))
    (setq result-list (cdr result-list))
    (mapcar (lambda (x)
              (setq or-res (union or-res x))) result-list)
    or-res))
    

;only one list because and/or operation before negation
(defun negate (result)
  (let ((all-docs '()))
    (loop for id being the hash-key of *documents* do
          (setq all-docs (cons id all-docs)))
    (setq all-docs (set-difference all-docs result))
    all-docs))


                        
                               

;(negate '(1 2 3))
;(connect-or '((1 2 3 4 7) (2 3 4)))
;(connect-and '((1 2 3) (4 2 8 3)))



        

;(recursive-list-search "drei" '("sdfkj" '(bla kansdfj "sjdfblaskdjfkd" "dredf") jfsdof 3 '(2 '('("drei")))))

;(num-search "drei" 3)
;(symbol-search "blacvbfdg" 'bla)
