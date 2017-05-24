
(defun on-add-clicked (text)
  (let ((selection-list) (free-text-results '()) (keyword-results '()) (no-error nil) (total-results '()))
    (setq selection-list (capi:choice-selected-items keyword-panel))
    (display-query selection-list text)
    ;free-text-search
    (cond ((member 'FREITEXT selection-list) 
           (setq free-text-results (on-free-text-query-submitted text)) ;(score docID)
           ))
    ;keyword-search
    (setq tmp (verarbeite-query selection-list text))
    (cond ((second tmp) (setq keyword-results (first tmp)) (setq no-error t))) 
    ;get total result list for current query-term
    (cond (no-error     
           (setq total-results (merge-results free-text-results keyword-results))))


    (cond ((and (> (length *results*) 0) (> (length total-results) 0) (equal (capi:choice-selected-item buttons-outer-term) and-outer-term)) ;connect with compplete query
          (setq tmp (list total-results *results*))
          (setq *results* (connect-and-total tmp)))
         ((and (> (length *results*) 0) (> (length total-results) 0) (equal (capi:choice-selected-item buttons-outer-term) or-outer-term))
          (setq tmp (list total-results *results*))
          (setq *results* (connect-or-total tmp)))
         (t (setq *results* total-results))))) ;nothing to connect with






(defun merge-results (free-text-results keyword-results)
  (mapcar (lambda (x)
           (setq old-score (car (member x free-text-results :test '= :key 'second)))
           (cond ((not (equal nil old-score)) 
                  (setq new-score (list (+ 1 (car old-score)) (second old-score))) ;existing vector-space score + 1 (for succesful keyword search)
                  (replace old-score new-score))
                 (t (setq free-text-results (cons (list 1 x) free-text-results))))) keyword-results)  ;add new result entey with score 1 
          free-text-results)


;todo: add existing scores together and sort after scores
(defun connect-and-total (res) 
 (let ((smaller-list '()) (larger-list '()) (total '()))
   (setq res (sort res '< :key 'length))
   (setq smaller-list (first res))
   (setq larger-list (second res))
   (mapcar (lambda (x)
             (setq tmp (car (member (second x) larger-list :test '= :key 'second)))
             (cond ((not (equal nil tmp)) 
                    (setq new (list (+ (first x) (first tmp)) (second tmp))) ;add scores 
                    (setq total (cons new total))))) smaller-list)))


            


(defun connect-or-total (res) 
 (let ((smaller-list '()) (larger-list '()) (elem) (tmp))
   (setq res (sort res '< :key 'length))
   (setq smaller-list (first res))
   (setq larger-list (second res))
   (mapcar (lambda (x)
             (setq elem (member (second x) larger-list :test '= :key 'second))
             (setq tmp (car elem))
             (cond ((not (equal nil elem)) 
                    (setq new (list (+ (first x) (first tmp)) (second tmp))) ;add scores 
                    (replace tmp new))
                   (t (setq larger-list (cons x larger-list))))) smaller-list)
   (setq larger-list (sort larger-list '> :key 'car))))




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
    (setq top (sort top '> :key 'car))
    (mapcar (lambda (x) 
              (setq score (write-to-string (first x)))
              (setq score (subseq score 0 (min (length score) 7)))
              (setq tmp (concatenate 'string (write-to-string index) ".      " score "        "  
                                           (write-to-string (document-path (gethash (second x) *documents*)))))
              (setq string-list (cons tmp string-list))
              (setq index (- index 1))) top)
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








;(connect-and-total '(((0.2 15) (0.3 2)) ((0.2 4) (0.3 2))))
;(connect-or-total '(((0.2 15) (0.3 2)) ((0.2 4) (0.3 2))))

(setq string (namestring (document-path (gethash 4 *documents*))))
 (dir string)