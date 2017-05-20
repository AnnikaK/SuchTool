(defstruct result keyword docs)

(defun on-add-clicked (text)
  (let ((selection-list) (free-text-results '()) (keyword-results '()) (no-error nil))
    (setq selection-list (capi:choice-selected-items keyword-panel))
    (display-query selection-list text)
    ;free-text-search
    (cond ((member 'FREITEXT selection-list) 
           (setq free-text-results (on-free-text-query-submitted text)) ;(score docID)
           (show free-text-results)))
    ;keyword-search
    (setq tmp (verarbeite-query selection-list text))
    (print tmp)
    (cond ((second tmp) (setq keyword-results (first tmp)) (setq no-error t))) 
    (cond (no-error 
           (mapcar (lambda (x) (if (not (member x free-text-results))
                                   (setq free-text-results (cons (list 1 x) free-text-results)))) keyword-results)
           (show free-text-results)))))

    


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


(defun show (top)
  (let ((string-list '()) (tmp) (index (length top)) (score))
    (mapcar (lambda (x) 
             (cond ((equal (first x) 1) (setq score "-")) 
                   (t (setq score (write-to-string (first x)))
                      (setq score (subseq score 0 (min (length score) 7)))))
             (setq tmp (concatenate 'string (write-to-string index) ".      " score "        "  
                                    (write-to-string (document-path (gethash (second x) *documents*)))))
             (setq string-list (cons tmp string-list)) (setq index (- index 1))) top)
    (show-results string-list)))



(defun show-results (result-list)
  (let ((show '("No results found.")) (breite))
    
    
    (cond ((not (equal '() result-list)) (setq show  result-list)))
    (setq breite (+ 30 (round (* 7.5 (reduce 'max (mapcar 'length show))))))
   
    (capi:display-dialog  
     (capi:make-container
      (make-instance 'capi:column-layout
                     :x-adjust  :center
                     :min-width breite 
                     :description
                     (list 
                     
                           (make-instance 
                            'capi:row-layout :description 
                            (list (make-instance 'capi:column-layout
                                                 :x-adjust :left
                                                 :min-width breite  
                                                 :background  :white :gap 5
                  :foreground :reda
                                                 :description show)))
                           "" 
                           (make-instance
                            'capi:push-button
                            :text "OK"  
                            :callback-type :data
                            :callback 
                            #'(lambda (arg)  
                                (capi:exit-dialog arg)))
                           ))
      :title "RESULTS"
      :min-width breite
      :background  :white
      ))))


(defun show-error ()
  (capi:display-dialog  
     (capi:make-container
      (make-instance 'capi:column-layout
                     :x-adjust  :center
                     :width 200
                     :description
                     (list 
                     
                           (make-instance 
                            'capi:row-layout :description 
                            (list "Please select at least one keyword!" ""))
                           (make-instance
                            'capi:push-button
                            :text "OK"  
                            :callback-type :data
                            :callback 
                            #'(lambda (arg)  
                                (capi:exit-dialog arg))))
      :min-width 200
      :background  :white
      ))))







