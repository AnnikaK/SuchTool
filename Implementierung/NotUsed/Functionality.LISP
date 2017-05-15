(defun displaySelection (radioButtonPanel)
  (setq tmp (capi:choice-selected-item radioButtonPanel))
  (if (equal tmp buttonAnd) 
      (print "AND")
    (print "OR")))


(defun getAllSelectedKeywords (checkButtonPanel)
  (let* ((answer '()) (boxes (capi:collection-items checkButtonPanel)) (size (length boxes)))
    (loop for x from 0 to size do
          (if (capi:button-selected (vector-pop boxes)) (setq answer (append answer (list x))))) boxes))


(defun datei-lesen-in-einen-string (dateiname &key (max-anz nil))
; input: ein dateiname (vollstŠngiger Pfad)
; value: ein String.
  (if (probe-file dateiname)
      (do* ((streamin (open dateiname)) (n 0 (+ n 1))
            (expr (read-char streamin nil 'eof) (read-char streamin nil 'eof))
            (exprs nil))
           ((or (equal expr 'eof) (and max-anz (> n max-anz)))
            (close streamin) (if exprs (map 'string 'identity (nreverse exprs)) " "))
        (setq exprs (cons (if (member (char-code expr) '(10 13)) #\space expr) exprs)))
    " "))




;;;Functionality


;UGLY VARIANTE 
(defun make-string-list (datei-string)
  (do ((index 0 (+ 1 index))(string-liste '())(brackets 0))do;init vars
      ((= index (length datei-string)) string-liste) ;condition + return 
       (cond 
             ((equal (char datei-string index) '#\) )(setq brackets (- brackets 1))) ;closing bracket
             ((equal (char datei-string index) '#\( )(setq brackets (+ brackets 1)) ;open bracket 


              (cond  ((equal brackets 1)  ;new open bracket?
                      (setq new-string "") 
                      (setq index (+ 1 index)) ;don't want the bracket 
                      (loop while (and (< index (length datei-string))(not (equal (char datei-string index) '#\Space))) do ;iterate over keyword
                          (setq new-string (concatenate 'string  new-string (string (char datei-string index))))
                          (setq index (+ 1 index)))
                      (cond (( > (length new-string) 0)
                             (setq key (intern new-string))
                             (cond ((not (gethash key *keyword-dict*)) ;check if keyword already existing
                                    (setq string-liste (append string-liste (list new-string))) ;add string to item-list for UI
                                    (setf (gethash key *keyword-dict*) -1)))))))))))  ;add keyword to dictionary




;BETTER BUT DOES NOT WORK YET
;create keyword-dict + item list for ui + later save words/free text
(defun verarbeite-datei-zeilenweise (dateiname &key (max-anz nil))
  (if (probe-file dateiname)
      (do* ((streamin (open dateiname))(current-line (read-line streamin nil 'eof)(read-line streamin nil 'eof))(string-liste '())) ;vars
           ((equal current-line 'eof)(close streamin) string-liste) ;return if end of file
        
        (cond ((equal (char current-line 0) '#\( ) ;zeile enthaelt keyword
               (setq index 1) ;don't want the bracket
               (setq keyword-string "")
               (loop while (and (< index (length current-line))(not (equal (char current-line index) '#\Space ))) do  ;get keyword
                           (setq keyword-string (concatenate 'string keyword-string (string (char current-line index)))) 
                           (setq index (+ 1 index)))
               (cond ( (> (length keyword-string) 0) ;really new keyword?
                        ;create symbol 
                       (setq key (intern keyword-string))
                       (cond ((not (gethash  key *keyword-dict*)) ;no duplicates
                              (setq string-liste (append string-liste (list keyword-string))) ;add to item list UI
                              (setf (gethash key *keyword-dict*) -1)))))))) NIL)) ;add to hash table




 ;save all documents indexed in hash table
(defun make-dokument-list(directory-name)
  (setq document-list (make-hash-table)) ;(dokumente mit ID als key und path als value
  (setq index 0)
 (mapcar (lambda (x) 
           (setf (gethash index document-list)(namestring x)) ;path in docID speichern 
           (setq index (+ 1 index)))
         (directory directory-name))
 (generateKeywords document-list))



(defun generateKeywords (document-list)
  (setq string-liste '())
  (setq keyword-liste (make-hash-table))
  (loop for key being the hash-key of document-list do ;for each document
        (setq  datei-string (datei-lesen-in-einen-string (gethash key document-list))) 
        ;alles ab hier ist toImprove!
        (setq brackets 0)

        (loop for index from 0 to (- (length datei-string) 1) do
              (cond 
               ((equal (char datei-string index) '#\) )(setq brackets (- brackets 1))) ;closing bracket
               ((equal (char datei-string index) '#\( )(setq brackets (+ brackets 1)) ;open bracket 


                (cond  ((equal brackets 1)  ;new open bracket?
                      (setq new-string "") 
                      (setq index (+ 1 index)) ;don't want the bracket 
                      (loop while (and (< index (length datei-string))(not (equal (char datei-string index) '#\Space))) do ;iterate over keyword
                          (setq new-string (concatenate 'string  new-string (string (char datei-string index))))
                          (setq index (+ 1 index)))
                      (cond (( > (length new-string) 0)
                             (setq current-keyword (intern new-string))
                             (if (not (gethash current-keyword keyword-liste)) ;check if keyword already existing
                                    (progn (setq string-liste (append string-liste (list new-string))) ;add string to item-list for UI
                                      (setf (gethash current-keyword keyword-liste) (list key)))
                               (setf (gethash current-keyword keyword-liste) (append (gethash current-keyword keyword-liste) (list key)))))))))))) string-liste)





(defun generateKeywords2 (document-list)
  (setq string-liste '())
  (setq keyword-liste (make-hash-table))
  (loop for key being the hash-key of document-list do ;for each document
        (setq  datei-string (datei-lesen-in-einen-string (gethash key document-list)))
        (setq datei-string (make-string-input-stream datei-string))

        (loop while (not (equal (read-line datei-string nil 'eof) nil))  do
              (setq current-line (read-line datei-string nil 'eof))
              (cond ((equal (char current-line 0) '#\( ) ;zeile enthaelt keyword
                     (setq index 1) ;don't want the bracket
                     (setq keyword-string "")
                     (loop while (and (< index (length current-line))(not (equal (char current-line index) '#\Space ))) do  ;get keyword
                           (setq keyword-string (concatenate 'string keyword-string (string (char current-line index)))) 
                           (setq index (+ 1 index)))
                     (cond ((> (length keyword-string) 0) ;really new keyword?
                        ;create symbol 
                       (setq current-keyword (intern keyword-string))
                       (if (not (gethash  current-keyword keyword-liste)) ;no duplicates
                             (progn (setq string-liste (append string-liste (list keyword-string))) ;add to item list UI
                              (setf (gethash current-keyword keyword-liste) (list key)))
                         (setf (gethash current-keyword keyword-liste) (append (gethash current-keyword keyword-liste) (list key)))))))))) string-liste)







                               
        




;;;;TESTING STUFF
(defun printDict (dict)
  (loop for key being the hash-key of dict do 
        (print key)(print (gethash key dict))))





(verarbeite-datei-zeilenweise "C:\\Users\\kreme\\OneDrive\\PractisingLisp\\Lisp\\mail-beispiele-eingang\\mail-beispiele-eingang\\mail-583.ldd")

(make-dokument-list "C:\\Users\\kreme\\OneDrive\\PractisingLisp\\Lisp\\mail-beispiele-eingang\\mail-beispiele-eingang\\")



