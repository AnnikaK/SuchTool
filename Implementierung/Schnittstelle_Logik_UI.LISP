(defstruct result keyword docs)

(defun on-add-clicked (text)
  (let ((selection-list))
    (setq selection-list (capi:choice-selected-items keyword-panel))
    (display-query selection-list text)
    ;free-text-search
    (cond ((member 'FREITEXT selection-list) 
           (on-query-submitted text)))
    ;keyword-search
    (verarbeite-query selection-list text)))


;show query in display-panel (without any logic!!!)
(defun display-query (selection-list text)
  (let ((and t) (displayed-query) (tmp "") (before "") (middle "") (after ""))
    (cond ((equal (capi:choice-selected-item radio-buttons) button-or) (setq and nil))) 
    (setq displayed-query (capi:display-pane-text query-display))
    ;verknuepfung intern
    (cond ((> (length selection-list) 1)
           (if and (setq before " (and ") (setq before " (or "))
           (setq after ") ")))
    ;surround with not
    (cond ((equal '(not) (capi:choice-selected-items not-operator-panel))
           (setq before (concatenate 'string " (not " before))
           (setq after (concatenate 'string after ") "))))
    
     ;create bracket-expression for each keyword
    (mapcar (lambda (x) 
              (setq tmp (concatenate 'string " (= " (write-to-string x) " \""  text  "\") "))
              (setq middle (concatenate 'string middle tmp))) selection-list)
    (setq tmp (concatenate 'string before middle after))
    ;verknuepfung extern
    (cond ((> (length displayed-query) 0)
           (cond ((equal (capi:choice-selected-item buttons-outer-term) and-outer-term)(setq displayed-query (concatenate 'string "(AND " displayed-query)))
                 ((equal (capi:choice-selected-item buttons-outer-term) or-outer-term) (setq displayed-query (concatenate 'string "(OR " displayed-query))))
           (setq tmp (concatenate 'string tmp ")"))))

    (setf (capi:display-pane-text query-display) (concatenate 'string displayed-query tmp))))





;logical procession of query
(defun verarbeite-query (selection-list text)
  (let ((entry-list) (entry) (type) (content) (result-list '()))
    (mapcar (lambda (keyword)
              (setq entry-list (gethash keyword *keyword-dict*))
              (setq keyword-result '()) ;alle dokumente, bei denen das keyword den suchtext enthaelt
              (mapcar (lambda (entry) 
                        (setq docID (keyword-entry-doc-id entry))
                        (setq type (keyword-entry-keyword-type entry))
                        (setq content (keyword-entry-content entry))
                        (cond ((and (equal type 'cons) (list-search text content)) (setq keyword-result (cons docID keyword-result)))
                              ((and (equal type 'simple-base-string) (string-search text content)) (setq keyword-result (cons docID keyword-result)))
                              ((and (equal type 'fixnum) (num-search text content)) (setq keyword-result (cons docID keyword-result))))) entry-list)
              (setq result-list (cons keyword-result result-list))) selection-list)
    (if (equal (capi:choice-selected-item radio-buttons) button-or) (setq result-list (connect-or result-list))
      (setq result-list (setq result-list (connect-and result-list))))
    (if (equal '(not) (capi:choice-selected-items not-operator-panel)) (setq result-list (negate result-list))) 
    (print result-list)))




(defun list-search (text content)
  (or (member text content) 
      (member (ignore-errors (read-from-string text)) content)))

(defun string-search (text content)
  (search text content))


(defun num-search (text content)
  (setq num (ignore-errors (read-from-string text)))
  (cond ((numberp num) (= num content))
        (t nil)))


(defun connect-and (result-list)
 (let ((and-res))
  (setq result-list (sort result-list '< :key 'length))
  (setq and-res (car result-list))
  (setq result-list (cdr result-list))
  (mapcar (lambda (x) 
            (setq and-res (intersection and-res x))) result-list)
  (setq and-res (sort and-res '<))))
    


(defun connect-or (result-list)
  (let ((or-res))
    (setq or-res (car result-list))
    (setq result-list (cdr result-list))
    (mapcar (lambda (x)
              (setq or-res (union or-res x))) result-list)
    (setq or-res (sort or-res '<))))
    

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

