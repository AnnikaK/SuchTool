(capi:define-interface swgui==schlagwort-interface () 
  ()    
  (:layouts
   (default-layout
    capi:column-layout
    (list 
     
     "Select directory:" 

      (make-instance
       'capi:row-layout :gap 20
       :description
       (list 
             (setq path-text (make-instance 'capi:display-pane :text "C:\\" :min-width 900 :max-width 900))
             (make-instance 'capi:button :text "select" :callback #'(lambda (&rest args)(let (antwort)
                                                                                           (setq antwort (capi:prompt-for-directory "Select directory" )) 
                                                                                           (cond (antwort 
                                                                                                  
                                                                                                  (setq antwort (namestring antwort))
                                                                                                  (setf (capi:display-pane-text path-text) antwort)
                                                                                                  (setq itemList (onSelected antwort))

                                                                                                  (setf (capi:collection-items keyword-panel) itemList))))))))
          
    



"Query:"


     ;row
     (make-instance 
      'capi:row-layout :gap 20
      :description
      (list (setq query-display
                  (make-instance  'capi:display-pane   :text ""  :min-height 100 :max-height 100 :min-width 900 :max-width 900))
            (make-instance  'capi:button :text "search" 
                            :callback #'(lambda (&rest args) ))   
             (make-instance  'capi:button :text "reset" 
                            :callback #'(lambda (&rest args) (setf (capi:display-pane-text query-display) "") (setq *results* #()))) ;wenn anfrage resetted dann auch resultate hierfuer!
            ;make-instance  'capi:button :text "quit" 
                           ; :callback #'(lambda (&rest args) (quit)))

            ))


(make-instance 
      'capi:row-layout :gap 20
      :description
      (list "Connect multiple query-terms with: "
             (setq buttons-outer-term (make-instance 
                                  'capi:radio-button-panel
                                  :items (list 
                                          (setq and-outer-term (make-instance 'capi:radio-button
                                                         :text "AND"
                                                         :selected t
                                                         :selection-callback 
                                                         #'(lambda  (a interface &rest args) 
                                                             (setq radio-button 1))))
                                          (setq or-outer-term (make-instance 'capi:radio-button
                                                         :text "OR"
                                                         :selected nil
                                                         :selection-callback 
                                                         #'(lambda  (a interface &rest args) 
                                                             (setq radio-button 2)))))))))

"" ""


 (make-instance 'capi:row-layout :gap 20 :description 
                (list 
                 (setq search-field (make-instance 
                  'capi:text-input-pane 
 
                  :text "search for..."
                  :min-width 900
                  :max-width 900
                  :min-height 100
                  :max-height 100
                  :change-callback #'(lambda (text &rest args)          
                                (setq *text* text))))


 (make-instance 'capi:column-layout :gap 20 :description 
                (list 

                 (make-instance 'capi:row-layout :gap 20 :description 
                                (list 
                                 (make-instance 
                                  'capi:button 
                                  :text "reset" 
                                  :callback #'(lambda (&rest args) 
                                  (setf (capi:text-input-pane-text search-field) "")))))
                 
                 
                 (make-instance 'capi:row-layout :gap 20 :description 
                                (list 
                                 (setq radio-buttons (make-instance 
                                  'capi:radio-button-panel
                                  :items (list 
                                          (setq button-and (make-instance 'capi:radio-button
                                                         :text "AND"
                                                         :selected t
                                                         :selection-callback 
                                                         #'(lambda  (a interface &rest args) 
                                                             (setq radio-button 1))))
                                          (setq button-or (make-instance 'capi:radio-button
                                                         :text "OR"
                                                         :selected nil
                                                         :selection-callback 
                                                         #'(lambda  (a interface &rest args) 
                                                             (setq radio-button 2)))))))
                            
                                 (make-instance 
                                  'capi:button 
                                  :text "add" 
                                  :callback #'(lambda (&rest args) 
                                                (on-add-clicked (capi:text-input-pane-text search-field))
                                                ;(on-query-submitted (capi:text-input-pane-text search-field))

                                            ))))

     (setq not-operator-panel (make-instance 'capi:check-button-panel
                                                                                     :min-width 900
                                                                                     :layout-class 'capi:column-layout
                                                                                     :items   '(NOT)))




))))  
                



"" "Keys:"


              (make-instance 'capi:column-layout 
                             :vertical-scroll t
                             :description (list
                                           (setq keyword-panel (make-instance 'capi:check-button-panel
                                                                                     :min-width 900
                                                                                     :layout-class 'capi:column-layout
                                                                                     :items   nil))))





              
              )
    :gap 20
    :x 0
    :min-width 1124;'(- :screen-width 50)
    :min-height 800
    :y 0
    :x-adjust :left
    :internal-border 20
    )))




(defun swgui++oeffnen (&rest args) ; (setq args nil)

  (let ()

    (cond ((and (boundp 'swgui==schlagwoerter-fenster) swgui==schlagwoerter-fenster)
           (capi:apply-in-pane-process swgui==schlagwoerter-fenster 'capi:destroy swgui==schlagwoerter-fenster)))
    (setq swgui==schlagwoerter-fenster
          (capi:display 
           (make-instance 
            'capi:interface
            :title "SuchTool"
            :x 20
            :y 100
            :layout (list
                      (make-instance 'capi:row-layout 
                                    :description
                                    (list 
                                     (make-instance 'swgui==schlagwort-interface) 
                                     ))))))))









(swgui++oeffnen)


