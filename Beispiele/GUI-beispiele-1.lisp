
; Datei, Ordner w�hlen:

(let (antwort)
  (setq antwort (capi:prompt-for-file "Datei ausw�hlen" ))
  (Cond (antwort 
	 (Setq antwort (namestring antwort))
         )))

(let (antwort)
  (setq antwort (capi:prompt-for-directory "Ordner ausw�hlen" ))
  (Cond (antwort 
	 (Setq antwort (namestring antwort))
         )))

; einfache GUI:

(defun gui-1 (&rest zeilen)
  (let (breite)
    (setq breite (+ 30 (round (* 8.5 (reduce 'max (mapcar 'length zeilen))))))
    (capi:display-dialog  ; oder (capi:display
     (capi:make-container
      (make-instance 'capi:column-layout
                     :x-adjust  :center
                     :min-width breite 
                     :background  :orange ; :green ; :white
                 ; :foreground :red
                     :description
                     (list (make-instance 'capi:title-pane :text "eingegebene Zeilen:")
                           (make-instance 
                            'capi:row-layout :description 
                            (list (make-instance 'capi:column-layout
                                                 :x-adjust :left ; :center
                                                 :min-width breite  
                                                 :background  :orange :gap 5
                 ; :foreground :red
                                                 :description zeilen)))
                           "" 
                           (make-instance
                            'capi:push-button
                            :text "OK"  
                            :callback-type :data
                            :callback 
                            #'(lambda (arg)  
                                (capi:exit-dialog arg)))
                           ))
      :title "GUI-1"
      :background  :orange
  ; :x-adjust  :center
      :min-width breite
      ))))

(gui-1 "zeile1" "lasdfas                         gasdf" "" "Zeile4")

; weitere Aufrufe: 

(capi:contain
 (make-instance 
  'capi:radio-button-panel
  :items (list 
          (make-instance 'capi:radio-button
                         :text "radio-button 1"
                         :selected t
                         :selection-callback 
                         #'(lambda  (a interface &rest args) 
                             (setq radio-button 1)))
          (make-instance 'capi:radio-button
                         :text "radio-button 2"
                         :selected nil
                         :selection-callback 
                         #'(lambda  (a interface &rest args) 
                             (setq radio-button 2))))))

(capi:contain
 (make-instance 
  'capi:check-button :text "Check-Button"
  :min-width 200 :max-width 200
  :selection-callback #'(lambda (data interface) 
                          (Setq gesetzt t))
  :retract-callback   #'(lambda (data interface) 
                          (Setq gesetzt nil))
  :selected nil))


(capi:contain
 (make-instance 'capi:row-layout :description 
                (list 
                 (make-instance 
                  'capi:text-input-pane 
                  :text "" 
                  :change-callback #'(lambda (text &rest args) 
                                (setq *text* text)))
                 (make-instance 
                  'capi:button 
                  :text "test" 
                  :callback #'(lambda (&rest args) 
                                (print "button"))))))


