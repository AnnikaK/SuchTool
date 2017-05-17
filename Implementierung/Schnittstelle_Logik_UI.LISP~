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




(defun verarbeite-query (selection-list text)
  (let ((entry-list) (entry) (type) (content) (result-list))
    (mapcar (lambda (keyword)
              (setq entry-list (gethash keyword *keyword-dict*))
              (setq keyword-result '())
              (mapcar (lambda (entry)    
                        (setq type (keyword-entry-keyword-type entry))
                        (setq content (keyword-entry-content entry))
                        (cond ((equal type 'cons)
                               (if (list-search text content) (setq keyword-result cons keyword-entry-doc-id keyword-result)))
                               ((equal type 'simple-base-string) 
                                (if (string-search text content)
                                   (setq keyword-result cons keyword-entry-doc-id keyword-result)))
                               ((equal type 'fixnum)
                                (if (number-search text content) (setq keyword-result cons keyword-entry-doc-id keyword-result))))) entry-list)
              (setq result-list (cons keyword-result result-list))) selection-list)
    result-list))
    
    


    
    

(defun list-search (text content)
  (member text content))
    



(defun string-search (text content)
  (subseq text content))


(defun number-search (text content)
  (= text content))



    
    
    
   
