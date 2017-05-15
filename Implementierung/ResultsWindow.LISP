
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



