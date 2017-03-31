#|

Auszug aus einer Anwendung, nur teilweise auswertbar, 
mit Beispielen zu "option-pane" und "multi-column-list-panel"

die Datei kann ganz ausgewertet werden, dann öffnet sich ein Fenster.
Wenn bei "Auswahl-Alternativen" der letzte Punkt "Stoppwort-Liste" gewählt wird,
werden in der Tabelle eine Zeilen gebildet. 

In der Tabelle kann ein Wert gewählt und z.B. mit der rechten Maus "Aktion setzen" gewählt werden. 

Die sonstigen möglichen Aktionen führen auf einen Fehler, weil die jeweiligen Funktionen nicht definiert sind. 

Der nachfolgende Code kann als Muster für sonstige Anwendungen verwendet werden. 



|#


(capi:define-interface swgui==schlagwort-interface ()
  ()    
  (:layouts
   (default-layout
    capi:column-layout
    (list 
     (setq swgui==schlagwoerter-auswahl (make-instance
      'capi:option-pane
      :items '("Verwendete Schlagworte" "Verwendete Phrasen" "Beck-Online-Schlagwortwolke" "Beck-Online-Suchbegriffe"
                                        "Stoppwort-Liste")
      :selected-item "Verwendete Schlagworte"
      :title "Auswahl-Alternativen:"
      :selection-callback #'(lambda (&rest args) (swgui++schlagwort-liste-auswaehlen (first args)))))
     (make-instance 
      'capi:row-layout :gap 20
      :description
      (list (setq swgui==schlagwoerter-suchtext
                  (make-instance  'capi:text-input-pane :title "Such-Text:" :text "" :min-width 300 :max-width 300))
            (make-instance  'capi:button :text "mit Suchtext neu laden" 
                            :callback #'(lambda (&rest args) (swgui++suche-trefferliste-einschraenken)))
            (make-instance  'capi:button :text "Gesamtliste neu laden" 
                            :callback #'(lambda (&rest args) (swgui++suche-trefferliste-wiederherstellen)))
            ))
     (make-instance 
      'capi:row-layout :gap 20
      :description
      (list (make-instance  'capi:button :text "sichern" 
                            :callback #'(lambda (&rest args) (swgui++sichern)))
            "    Anzahl:  "
            (setq swgui==schlagwoerter-anzahl
                  (make-instance  'capi:title-pane :text "" :min-width 100 :max-width 100))
            "Auswahl:  "
            (setq swgui==schlagwoerter-auswahl
                  (make-instance  'capi:title-pane :text "" :min-width 300 :max-width 300))
            (make-instance  'capi:button :text "ausführen" 
                            :callback #'(lambda (&rest args) (SB++AUSFUEHREN-DIALOG)))
            (make-instance  'capi:button :text "beenden" 
                            :callback #'(lambda (&rest args) (quit)))))
     (setq swgui==test-scroll-tabelle
      (make-instance 
      'capi:column-layout :gap 20 :horizontal-scroll t
      :description
      (list
     (Setq swgui==schlagwoerter-trefferliste
           (make-instance  'capi:multi-column-list-panel 
                           :name "Ergebnisse"
                           :min-height 200   :vertical-scroll t :horizontal-scroll t
                             :interaction :extended-selection
                     ;:min-width '(- :screen-width 50)
    ; :max-height '(* :screen-height 0.3)
  ; :selection-callback #'(lambda (&rest x) (break "selection header trefferliste") )
   ; :extend-callback #'(lambda (&rest x) (break "selection header trefferliste1") )
   ; :action-callback #'(lambda (&rest x) (break "selection header trefferliste2") )
  ;  :interaction :extended-selection
     :header-args `(:callback-type :data
                   :selection-callback ,#'(lambda (&rest args)
                                            ;(nth 0 '(0 1 2 3))   ; (setq args '("Datum"))
                                            (let (sortiere-nach);(setq sortiere-nach 1)
                                              ; (print args)
                                              ; (print (position (read-from-string (car args)) 
                                              ;                  '(Beleg Typ Seiten Datum Klasse weitere Titel Autor Schlagwörter)))
                                                  (cond ((equal swgui**schlagwort-sort-funktion 'string<) 
                                                         (setq swgui**schlagwort-sort-funktion 'string>)
                                                         (setq swgui**schlagwort-anz-sort-funktion '>))
                                                        (t (setq swgui**schlagwort-sort-funktion 'string<)
                                                           (setq swgui**schlagwort-anz-sort-funktion '<)))
                                                  (setq sortiere-nach  ; (setq sortiere-nach 1 args '("Schlagwort"))
                                                        ; (setq sortiere-nach 2 args '("Anzahl"))
                                                        (position  (car args)
                                                                   '("sym" "Anzahl" "Schlagwort"  "Aktion" "Beispiele" )
                                                                   :test 'string-equal))
                                                  ; (length (capi:collection-items swgui==schlagwoerter-trefferliste))
                                                  (cond ((< (length (capi:collection-items swgui==schlagwoerter-trefferliste))
                                                            swgui**sort-max-anzahl)
                                                         (setf (capi:collection-items swgui==schlagwoerter-trefferliste)
                                                               (sort (capi:collection-items swgui==schlagwoerter-trefferliste)
                                                                     #'(lambda (x y)
                                                                         (cond ((string-equal (first args) "Anzahl") 
                                                                                (funcall swgui**schlagwort-anz-sort-funktion
                                                                                         (nth sortiere-nach x)
                                                                                         (nth sortiere-nach y)))
                                                                               (nil 
                                                                                (funcall swgui**schlagwort-sort-funktion 
                                                                                         (prin1-to-string (nth sortiere-nach x)) 
                                                                                         (prin1-to-string (nth sortiere-nach y))))
                                                                               (t 
                                                                                (funcall swgui**schlagwort-sort-funktion 
                                                                                         (nth sortiere-nach x) 
                                                                                         (nth sortiere-nach y))))))))
                                                        (t (meldung-ok  ""
                                                            "" "Die Liste ist zu groß und kann noch nicht sortiert werden."
                                                              )) ))))
    :columns '((:title "sym"
                :adjust :left
                :width (character 0))
               (:title "Anzahl"
                :adjust :left
                :width (character 20))
               (:title "Schlagwort"
                :adjust :left
                :width (character 60))
            
               (:title "Aktion"
                :adjust :left
                :width (character 20))
               (:title "Beispiele"
                :adjust :left
                :width (character 50))
                )
    :header-args '(;  :items ("Beleg" "Typ" "Seiten" "Datum" "Klasse" "weitere Klassen" "Titel" "Autor" "Schlagwörter"  )
                   :selection-callback test-spalte
                   )
    :items nil
    :horizontal-scroll t
    :vertical-scroll t
    :selection-callback #'(lambda (&rest args) 
                            (swgui++tabelle-selection-callback-fkt args))
                             ; (setq aitem (second args)) ;  (print (list 'sel args)))
    ; :visible-min-height '(:character 10)
    ; :visible-min-width '(:character 45)
    :pane-menu 'SWGUI++SCHLAGWORT-RECHTSKLICK)
     ))))
     (make-instance 
      'capi:column-layout :gap 20
      :description
      (list (make-instance 
             'capi:row-layout :gap 10
             :description
             (list "     "))))
     )
    :gap 5
    :x 0
    :min-width 1124;'(- :screen-width 50)
    :y 0
    :x-adjust :left
    :internal-border 4
    )))

(defun swgui++oeffnen (&rest args) ; (setq args nil)
  (let ()

    (cond ((and (boundp 'swgui==schlagwoerter-fenster) swgui==schlagwoerter-fenster)
           (capi:apply-in-pane-process swgui==schlagwoerter-fenster 'capi:destroy swgui==schlagwoerter-fenster)))
    (setq swgui==schlagwoerter-fenster
          (capi:display 
           (make-instance 
            'capi:interface
            :title "Schlagwort-Liste"
            :x 20
            :y 100
            :layout (list
                     (make-instance 'capi:row-layout
                                    :description
                                    (list
                                     (make-instance 'swgui==schlagwort-interface) 
                                     ))))))

    (swgui++refresh-swgui nil 'first (lambda (x) (get (first x) 'schlagwort-freq-1))
                          (lambda (x) (get (first x) 'schlagwort-freq-string)) (lambda (x) (or (get (first x) 'aktion) ""))
                          "Schlagworte in DB" "Verwendete Schlagworte")
   ; (length swgui**beck-online-wolke)  
  ; (length swgui**beck-online-suchbegriffe)                         
  ; (length kts**stoppwort-liste) 
  ; (length BSV**SCHLAGWORT-VORKOMMNISE)
  ; (length BSV**schlagwort-phrasen)

    ))

(defun swgui++schlagwort-liste-auswaehlen (auswahl &optional teilliste)
  (cond ((equal auswahl "Verwendete Schlagworte")
         (swgui++refresh-swgui (or teilliste BSV**SCHLAGWORT-VORKOMMNISE) 'first (lambda (x) (get (first x) 'schlagwort-freq-1))
                          (lambda (x) (get (first x) 'schlagwort-freq-string)) (lambda (x) (or (get (first x) 'aktion) ""))
                          "Schlagworte in DB" auswahl))
        ((equal auswahl "Verwendete Phrasen")
         (swgui++refresh-swgui (or teilliste BSV**schlagwort-phrasen) 'first (lambda (x) (get (first x) 'schlagwort-freq-1))
                          (lambda (x) (get (first x) 'phrasen-string)) (lambda (x) (or (get (first x) 'aktion) ""))
                          "Schlagwort-Phrasen in DB" auswahl))
        ((equal auswahl "Beck-Online-Schlagwortwolke")
         (swgui++refresh-swgui (or teilliste swgui**beck-online-wolke) 'identity (lambda (x) 0)
                          (lambda (x) (get x 'text)) (lambda (x) (or (get x 'aktion) "")) "Beck-Online Schlagwort-Wolke" auswahl))
        ((equal auswahl "Beck-Online-Suchbegriffe")
         (swgui++refresh-swgui (or teilliste swgui**beck-online-suchbegriffe) 'identity (lambda (x) 0)
                          (lambda (x) (get x 'text)) (lambda (x) (or (get x 'aktion) "")) "Beck-Online Suchbegriffe" auswahl))
        ((equal auswahl "Stoppwort-Liste")
         (swgui++refresh-swgui (or teilliste kts**stoppwort-liste) 'identity (lambda (x) 0)
                               (lambda (x) (get x 'stopp-wort)) (lambda (x) (or (get x 'aktion) "")) "Stopp-Wörter" auswahl)))
)

(defun swgui++refresh-swgui (sw-symbol-liste fkt-sym fkt-anz fkt-text  fkt-aktion auswahl-text auswahl)
  ; (setf (second (elt (capi:collection-items swgui==schlagwoerter-trefferliste) 5)) "test")
  ; (setf (capi:collection-items swgui==schlagwoerter-trefferliste) (capi:collection-items swgui==schlagwoerter-trefferliste))
  (setq swgui**auswahl-aktuell auswahl)
  (setf (capi:title-pane-text  swgui==schlagwoerter-anzahl) (prin1-to-string (length sw-symbol-liste)))
  (setf (capi:title-pane-text  swgui==schlagwoerter-auswahl) (prin1-to-string auswahl-text))
  (setf (capi:collection-items swgui==schlagwoerter-trefferliste) 
       ; ("red_nr" "anl_datum" "sprache" "RECHTSGEBIETE" "hinweis" "njnummer")
          (mapcar (lambda (x) ; (setq x (nth 0 test)) (setq x (nth 0 zz-symbol-liste))
                    (list (funcall fkt-sym x) (funcall fkt-anz x) (funcall fkt-text x)
                          (funcall fkt-aktion x) 
                          "" ; (or (prin1-to-string (get x 'anz)) "") 
                          ))
                  sw-symbol-liste))   
  )

(defun SWGUI++SCHLAGWORT-RECHTSKLICK (pane data x y)
  (declare (ignore  x y))
    ; (setq tp pane td data tx x ty y)  (break "rechtsklick") ; (setq pane tp data td x tx y ty)
  (let ( symbol)
    ; (print (setq selected-items (capi:choice-selected-items pane)))
    ; (setq list-name (capi:capi-object-name pane))
    ;  (setq typ (get (caar (capi:choice-selected-items (EPAGUI**layout-ergebnisse glob-interface))) 'typ))
    (cond ((and (consp data) (symbolp (first data)))
           (setq symbol (first data)))
          (t (setq symbol nil)))
    (make-instance 
               'capi:menu
               :items
               (list
                (make-instance 'capi:menu-component
                               :items
                               (list
                                (make-instance 
                                 'capi:menu-item
                                 :title "Aktion setzen"
                                 :callback-type :data
                                 ; :data imagefile
                                 :callback #'(lambda (menu)
                                               (cond ((and symbol  ;  10-h-regelung
                                                           (setq text-neu (text-eingabe "" "" "Aktion:")))
                                                      (setf (get symbol 'aktion-alt) (get symbol 'aktion))
                                                      (setf (get symbol 'aktion) text-neu)
                                                      (setf (fourth data) text-neu)
                                                      (setf (capi:collection-items swgui==schlagwoerter-trefferliste) 
                                                            (capi:collection-items swgui==schlagwoerter-trefferliste))))))
                                (make-instance 
                                 'capi:menu-item
                                 :title "NJ-Nummern zeigen"
                                  :callback-type :data
                                 ; :data imagefile
                                 :callback #'(lambda (menu)
                                               (cond (symbol
                                                      (text-anzeige "" "NJ-Nummern: " ""
                                                                    (get symbol 'nj)))
                                                     (t (meldung-ok "" "" "keine Zeile ausgewählt.")))
                                               ))
                                (make-instance 
                                 'capi:menu-item
                                 :title "Schlagworte zeigen"
                                  :callback-type :data
                                 ; :data imagefile
                                 :callback #'(lambda (menu)
                                               (cond (symbol
                                                      (text-anzeige "" "Schlagworte insgesamt: " ""
                                                                    (substitute #\; #\+ (get symbol 'schlagworte-gesamt))))
                                                     (t (meldung-ok "" "" "keine Zeile ausgewählt.")))
                                               ))
                      ))))))

(defun swgui++schlagwort-liste-auswaehlen (auswahl &optional teilliste)
  (cond ((equal auswahl "Verwendete Schlagworte")
         (swgui++refresh-swgui (or teilliste BSV**SCHLAGWORT-VORKOMMNISE) 'first (lambda (x) (get (first x) 'schlagwort-freq-1))
                          (lambda (x) (get (first x) 'schlagwort-freq-string)) (lambda (x) (or (get (first x) 'aktion) ""))
                          "Schlagworte in DB" auswahl))
        ((equal auswahl "Verwendete Phrasen")
         (swgui++refresh-swgui (or teilliste BSV**schlagwort-phrasen) 'first (lambda (x) (get (first x) 'schlagwort-freq-1))
                          (lambda (x) (get (first x) 'phrasen-string)) (lambda (x) (or (get (first x) 'aktion) ""))
                          "Schlagwort-Phrasen in DB" auswahl))
        ((equal auswahl "Beck-Online-Schlagwortwolke")
         (swgui++refresh-swgui (or teilliste swgui**beck-online-wolke) 'identity (lambda (x) 0)
                          (lambda (x) (get x 'text)) (lambda (x) (or (get x 'aktion) "")) "Beck-Online Schlagwort-Wolke" auswahl))
        ((equal auswahl "Beck-Online-Suchbegriffe")
         (swgui++refresh-swgui (or teilliste swgui**beck-online-suchbegriffe) 'identity (lambda (x) 0)
                          (lambda (x) (get x 'text)) (lambda (x) (or (get x 'aktion) "")) "Beck-Online Suchbegriffe" auswahl))
        ((equal auswahl "Stoppwort-Liste")
         (swgui++refresh-swgui (or teilliste kts**stoppwort-liste) 'identity (lambda (x) 0)
                               (lambda (x) (get x 'stopp-wort)) (lambda (x) (or (get x 'aktion) "")) "Stopp-Wörter" auswahl)))
)

(defun text-eingabe (zeile1 zeile2 zeile3 &key (farbe bs**meldung-background-farbe)  zeilen (text-inhalt ""))
  (let (inhalt-neu)
    (Setq inhalt-neu text-inhalt)
  (capi:display-dialog
   (capi:make-container
    (make-instance 'capi:column-layout
                   :x-adjust :center  :min-width 520  ;  :background farbe
                   :description
                   (nconc 
                    (list (make-instance 'capi:title-pane :text zeile1)  
                          (make-instance 'capi:title-pane :text zeile2)
                          (make-instance 'capi:title-pane :text zeile3)
                          )
                    (mapcar (lambda (x) (make-instance 'capi:title-pane :text x)) zeilen)
                    (list (make-instance 'capi:text-input-pane :text text-inhalt
                                                        :min-width 500 :max-width 500
                                                        :change-callback #'(lambda (text &rest args) 
                                                            (setq inhalt-neu text)))
                          ""
                          (make-instance 
                           'capi:row-layout :gap 20 :x-adjust :center
                           :description
                           (list
                            (make-instance
                             'capi:push-button
                             :text "OK" :callback-type :data
                             :callback 
                             #'(lambda (arg)    (capi:exit-dialog inhalt-neu)))
                            (make-instance
                             'capi:push-button
                             :text "Abbrechen" :callback-type :data
                             :callback 
                             #'(lambda (arg)    (capi:exit-dialog nil))))))))
    :title "Text-Eingabe"
    :min-width 520
    ))))

(setq bs**meldung-background-farbe :yellow)

(defun text-anzeige (zeile1 zeile2 zeile3 Text &key (farbe bs**meldung-background-farbe) )
  (let ()
    (capi:display-dialog
     (capi:make-container
      (make-instance 'capi:column-layout
                     :x-adjust :center  ; :min-width 520  
                     ; :background farbe
                     :description
                     (nconc 
                      (list (make-instance 'capi:title-pane :text zeile1)  
                            (make-instance 'capi:title-pane :text zeile2)
                            (make-instance 'capi:title-pane :text zeile3)
                            (make-instance 'capi:row-layout
                                           :description
                                           (list "          " (make-instance 'capi:title-pane :text Text) "          ")
                            ))
                    
                    (list ""
                          (make-instance 
                           'capi:row-layout :gap 20 :x-adjust :center
                           :description
                           (list
                            (make-instance
                             'capi:push-button
                             :text "OK" :callback-type :data
                             :callback 
                             #'(lambda (arg)    (capi:exit-dialog nil )))
                            )))))
    :title "Text-Anzeige"
    ; :min-width 520
    ))))

(setq stopp-woerter '("und" "oder" "nicht" "unten"))

(let (sym)
  (setq kts**stoppwort-liste (mapcar (lambda (x) (setq sym (read-from-string x))
                                       (setf (get sym 'stopp-wort) x)
                                       sym)
                                     stopp-woerter)))

(swgui++oeffnen)

