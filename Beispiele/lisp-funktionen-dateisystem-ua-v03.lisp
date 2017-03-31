;  �ffnen von Explorer-Fenster:
;  (sys:call-system "explorer F:\\Dokument-Verarbeitung\\EPA-Basis")
;  (sys:call-system "explorer /e,F:\\Dokument-Verarbeitung\\EPA-Basis")
;  (sys:call-system "explorer /select,F:\\Dokument-Verarbeitung\\EPA-Basis\\EPA-Basis-v05.lisp")

; der nachfolgende Ausdruck vermeidet Warnings bei neu-Definition  (evt. nur bei 5.0 ?)
; (setq dspec::*redefinition-action* :quiet)


(Defvar *laufwerk* (host-namestring (current-pathname)))

(defun btp (arg) (print (list (symbolp arg) (boundp arg) (eval arg))))

(defun do-n (n expr) (do ((z n (- z 1)))
                         ((< z 2) (eval expr))
                       (eval expr)))

(defun searchse (s1 s2) (search s1 s2 :test 'string-equal))

(defun dir (arg) (sys:call-system (cs "explorer /select," arg))) 

(defun zeit () (subseq (multiple-value-list (get-decoded-time)) 0 6))
(defun datum () (subseq (multiple-value-list (get-decoded-time)) 3 6))

(defun wochentag-nr () (seventh (multiple-value-list (get-decoded-time)) ))
(defun wochentag () (second (assoc (wochentag-nr) '((0 montag) (1 dienstag) (2 mittwoch) (3 donnerstag)
                                                    (4 freitag) (5 samstag) (6 sonntag)))))
(defun werktag () (< (wochentag-nr) 5))

(defun monat-name (monat) 
  (second (assoc monat '((1 "Januar")  (2 "Februar") (3 "M�rz")
                         (4 "April") (5 "Mai") (6 "Juni")
                         (7 "Juli")  (8 "August") (9 "September")
                         (10 "Oktober") (11 "November") (12 "Dezember")))))

(Defun copy-datei (in out)
  (system::copy-file in out)
  (SYSTEM:SET-FILE-DATES OUT :MODIFICATION (FILE-WRITE-DATE IN))
 )

(defun subset (funktion liste) (remove-if-not funktion liste)) 

(defun tag-aktuell-string () 
  (apply 'concatenate 'string 
         (map 'list (lambda (wert)
                      (concatenate 'string (if (< wert 10) "0" "") 
                                   (prin1-to-string wert)))                   
              (reverse (subseq (multiple-value-list (get-decoded-time)) 3 6)))))

(defun minzeit-aktuell () 
  (apply 'concatenate 'string 
         (map 'list (lambda (wert)
                      (concatenate 'string (if (< wert 10) "0" "") 
                                   (prin1-to-string wert)))                   
              (reverse (subseq (multiple-value-list (get-decoded-time)) 1 6)))))

(defun sekzeit-aktuell () 
  (apply 'concatenate 'string 
         (map 'list (lambda (wert)
                      (concatenate 'string (if (< wert 10) "0" "") 
                                   (prin1-to-string wert)))                   
              (reverse (subseq (multiple-value-list (get-decoded-time)) 0 6)))))

(Defun zeitliste-bilde-string (&optional zeitliste)
  (if (not zeitliste) (setq zeitliste (multiple-value-list (get-decoded-time ))))
  (let (sliste)
    (setq sliste (mapcar (lambda (x) (cond ((< x 10) (concatenate 'string "0" (prin1-to-string x)))
                                           (t (prin1-to-string x))))
                         (subseq zeitliste 0 5)))
    (concatenate 'string (fourth sliste) "." (fifth sliste) "." (prin1-to-string (sixth zeitliste)) "  "
                 (third sliste) ":" (second sliste) ":" (first sliste))))

(Defun datumliste-bilde-string (datumliste) ; (setq datumliste (datum ))
  (let (sliste)
    (setq sliste (mapcar (lambda (x) (cond ((< x 10) (concatenate 'string "0" (prin1-to-string x)))
                                           (t (prin1-to-string x))))
                         (subseq datumliste 0 3)))
    (concatenate 'string (first sliste) "." (second sliste) "." (third sliste) )))

(defun datum-get-sortstring (datum &key (trennstring ""))  ; (setq datum "3.2.08") (setq datum "30.04.2009  13:59:34")
  (cond ((> (length datum) 10) (setq datum (subseq datum 0 (position #\space datum)))))
  (let (pos1 pos2)
    (cond ((and datum
                (setq pos1 (position-if-not 'ist-ziffer datum))
                (setq pos2 (position-if-not  'ist-ziffer datum :start (+ 1 pos1)))
                )
           (setq jahr (subseq datum (+ 1 pos2) ))
           (cs (if (= 2 (length jahr)) "20" "") jahr trennstring ; 6 (min 10 (length datum))) 
               (if (= 2 (- pos2 pos1)) "0" "") (subseq datum (+ 1 pos1)  pos2) trennstring
               (if (<  pos1 2) "0" "") (subseq datum 0 pos1))))))

(defun unidatum-get-sortstring (datum) ; (setq datum 3491848800)
  (let (d2)
    (setq d2 (subseq (multiple-value-list (decode-universal-time datum)) 3 6))
    (cs (prin1-to-string (third d2)) (if (< (second d2) 10) "0" "") (prin1-to-string (second d2))
        (if (< (first d2) 10) "0" "") (prin1-to-string (first d2)))))

(defun datumstring-bilde-liste (datum)  ; (setq datum "20110818") (setq datum "12.5.2011")
  (let (strings liste)
    (setq strings (string-zerlegen-in-stringliste datum :trenn-code (char-code #\.)))
    (cond ((= 3 (length strings))
           (Setq liste (ignore-errors (mapcar 'read-from-string strings)))
           (if (every 'numberp liste) liste))
          ((and (= 1 (length strings)) (= 8 (length datum))
                (every 'ist-ziffer datum)
                (equal #\2 (elt datum 0)) (equal #\0 (elt datum 1)))
           (list (read-from-string (subseq datum 6 8))
                 (read-from-string (subseq datum 4 6))
                 (read-from-string (subseq datum 0 4)))))))
           

(defun zeitstring-bilde-liste (zeit)  
  (let (strings liste)
    (setq strings (string-zerlegen-in-stringliste zeit :trenn-code (char-code #\:)))
    (Setq liste (ignore-errors (mapcar 'read-from-string strings)))
    (if (every 'numberp liste) liste)))

(defun datum-sortdarstellung-get-print-darstellung (datum) ; (setq datum "20100805151810") (length datum)
  (cs (subseq datum 6 8) "."  (subseq datum 4 6) "."  (subseq datum 0 4) " "
      (subseq datum 8 10) ":"  (subseq datum 10 12) ":"  (subseq datum 12 14)))

(defun list<= (l1 l2)
  (and (= (length l1) (length l2))
       (list<=rek l1 l2)))

(defun list<=rek (l1 l2)
  (or (not l1)
      (and (numberp (first l1)) (numberp (first l2))
           (or (< (first l1)  (first l2))
               (and  (= (first l1)  (first l2))
                     (list<=rek (rest l1) (rest l2)))))))

(defun mengen-gleich (m1 m2 &key test )
  (if test 
      (and (equal (length m1) (length m2))
           (every (lambda (x) (member x m2 :test test)) m1)
           (every (lambda (x) (member x m1 :test test)) m2))
    (and (equal (length m1) (length m2))
         (every (lambda (x) (member x m2 )) m1)
         (every (lambda (x) (member x m1 )) m2))))


(Defun cs (&rest args) (eval (cons 'concatenate (cons ''string args))))

(defun bilde-string (expr) (if (stringp expr) expr (prin1-to-string expr)))


(defun ist-buchstabe (char) (or (<= 65 (char-code char) 90)
                                (<= 97 (char-code char) 122)
                                (member char '(#\� #\� #\� #\� #\� #\� #\�))))

(defun ist-grossbuchstabe (char) (or (<= 65 (char-code char) 90) ; (char-code #\A)
                                     (member char '( #\� #\� #\�))))

(defun ist-gross-buchstabe (z)  (or (< 64 (char-code z) 91)      
                                    (member (char-code z) '( 196 214 220 ))))
 
(defun ist-kleinbuchstabe (char) (or (<= 97 (char-code char) 122) ; (char-code #\A)
                                     (member char '( #\� #\� #\� #\� ))))

(defun ist-buchstabe-en (char) (or (<= 65 (char-code char) 90)
                                   (<= 97 (char-code char) 122)
                                ))

(defun ist-buchstabe-ohne-umlaute (z)
  (or (< 64 (char-code z) 91)
      (< 96 (char-code z) 123)
      ))

; (map 'list 'char-code "AZ���")


(defun ist-ziffer (char)   (<= 48 (char-code char) 57))
(defun ist-zahl-zeichen (char) (or (ist-ziffer char) (member char '(#\. #\-))))

(defun ist-zul-dateiname-zeichen (char) ; (char-code #\_)
  (let (code) (setq code (char-code char))
    (or (<= 48 code 57) 
        (<= 65 code 90)
        (<= 97 code 122)
        (member code '(45 95))
        )))

(defun ist-druckzeichen (z) (<= 32 (char-code z) 126))
(defun ist-umlaut (z) (member (char-code z) '(228 246 252 196 214 220 223)))

(Defun ist-satzzeichen (z) (member z '(#\! #\" #\' #\( #\) #\, #\. #\/ #\- #\: #\; #\? #\[ #\] )))

; (mapcar 'char-code '(#\! #\" #\' #\( #\) #\, #\. #\/ #\- #\: #\; #\? #\[ #\] ))

; (map 'list 'char-code "�������")
; (map 'list 'char-code ".-")

(defun dateien (platte)
 (delete-if (lambda (x)  (equal #\\ (elt x (- (length x) 1)))) (mapcar 'namestring (directory platte))))

(defun ordner (platte)
  (delete-if-not (lambda (x) (equal #\\ (elt x (- (length x) 1)))) (mapcar 'namestring (directory platte))))
	
(defun alle-dateien (platte &optional (bedingung 'identity))
  ; (setq platte "C:\\Dokumenten-Archiv-KH\\EPA2006\\EPA20060522\\" bedingung (lambda (x) (search ".sym" x)))
  (delete-if (function (lambda (datei)
              ; (setq datei (nth 0 (dateien "FPLATTE2:software:")))
                (equal #\. (elt (nur-dateiname datei) 0))))
          (nconc (dateien-bedingung platte  bedingung ) 
                 (reduce 'nconc (mapcar (lambda (x) ; (setq x (nth 4 (ordner platte)))
                                          (alle-dateien x bedingung)) (ordner platte) )))))

(defun alle-ordner (platte)
  (nconc (ordner platte) (reduce 'nconc (mapcar 'alle-ordner (ordner platte)))))

(defun dateien-typ (ordner endung)
  (cond ((stringp endung)
         (delete-if-not (lambda (x) (search endung x :test 'string-equal)) (dateien ordner)))
        ((consp endung)
         (delete-if-not (lambda (x) 
                          (some (lambda (y) 
                                  (search y x :test 'string-equal))
                                endung))
                        (dateien ordner)))))

(defun nur-dateiname (datei)
  (if (and (not (find #\\ datei)) (not (find #\/ datei))) datei
      (subseq datei  (+ (position-if (lambda (x) (member x '(#\\ #\/))) datei :from-end T)  1))))

(defun nur-ordnername (datei)
  (subseq datei 0 (+ (position-if (lambda (x) (member x '(#\/ #\\ #\:)))  datei 
                                :end (- (length datei) 1) :from-end T) 1)))

(defun nur-endname-von-ordnerpfad (datei)
  (string-trim "\\/" (subseq datei   (+ (position-if (lambda (x) (member x '(#\/ #\\ #\:)))  datei 
                                :end (- (length datei) 1) :from-end T) 1))))

(defun nur-endordner-von-ordnerpfad (datei)
  ; (setq datei "asdf/")  (setq datei "C:\\ADS-KH\\")
  (subseq datei   (+ (or (position-if (lambda (x) (member x '(#\/ #\\ #\:)))  datei 
                                  :end (- (length datei) 1) :from-end T)
                         -1)
                     1)))

(DEFUN DATEI-OHNE-ENDUNG (DATEI)
  (SUBSEQ DATEI 0 (or (POSITION #\. DATEI :FROM-END T)
                             (length datei))))

(DEFUN DATEI-nur-ENDUNG (DATEI)
  (SUBSEQ DATEI (or (POSITION #\. DATEI :FROM-END T)
                    "")))

(defun nur-pfad-zu-datei (datei)
  (if (and (not (find #\\ datei)) (not (find #\/ datei))) datei
      (subseq datei  0  (+ (position-if (lambda (x) (member x '(#\/  #\\))) datei :from-end T) 1))))

(defun pfad-get-platte (pfad)  ; (setq pfad "F:/asda") (setq pfad "Fplatte2:wer")
  (subseq pfad  0  (+ (position  #\: pfad) (if (ds++windows-p) 2 1))))

(defun pfad-ohne-platte (pfad)  ; (setq pfad "F:/asda") (setq pfad "Fplatte2:wer")
  (string-left-trim "/\\" (subseq pfad (+ (position  #\: pfad)   1))))

(defun alle-tiefsten-ordner (pfad) ; (setq pfad DS**Hauptarchiv-Pfad)
  (let (ord)
    (setq ord (alle-ordner pfad))
    (delete-if (lambda (x) (some (lambda (y) (and (not (equal x y)) (search x y))) ord)) ord)))


(defun nicht-leere-ordner (pfad)
  (delete-if (lambda (x) (and (not (dateien x)) (not (ordner x)))) (ordner pfad)))

(defun ordner-ist-leer-p (pfad) (and (not (dateien pfad)) (not (ordner pfad))))

(defun pfad-equal (p1 p2)
  ; (setq p1 "asdf" p2 "asdf/")
  (or (string-equal p1 p2) 
      (string-equal (substitute #\\ #\/ p1) (substitute #\\ #\/ p2))))

(defun pfadchar-equal (p1 p2)
  ; (setq p1 #\M p2 #\m)
  (or (char-equal  p1 p2)
      (and (equal p1  #\\) (equal p2 #\/ )) 
      (and (equal p2  #\\) (equal p1 #\/ ))))
      
(defun rename-ordner (o1 o2) 
  (rename-file o1 o2)
  )

(defun loesche-ordner (o1)  ;  Pr�fen !!!!    
  ; (setq o1 "C:\\DOKUMENTEN-ARCHIV-DEMO-I\\EPA-Wissensbasen-Ordnerstruktur\\ZURUECKSTELLEN\\TEST\\")
  (let (neu)
    (setq neu (nur-ordnername o1))
    (mapc (lambda (o) ; (setq o (nth 0 (ordner o1)))
            (rename-file o (cs neu (subseq o (length o1)))))
          (ordner o1))
    (mapc (lambda (o) ; (setq o (nth 0 (dateien o1)))
            (rename-file o (cs neu (subseq o (length o1)))))
          (dateien o1))
    (cond ((and (not (dateien o1)) (not (ordner o1)))
           (delete-directory  o1)))
  ))
   
(defun ordner-mit-dateien (pfad)
  (delete-if (lambda (x) (not (alle-dateien x))) (ordner pfad)))

(defun ordner-p (str)   
  (member (elt str (- (length str) 1)) '(#\\ #\/)))





(defun datei-lesen (dateiname) ; (Setq dateiname epai**ueberwacht-ordner-liste-dateiname)
  ; (setq dateiname "C:\\Archivinvoicekonradbau\\Belege\\EPA2011\\EPA20110926\\Ziegler_kronimus__ER_S-000.erg")
; input: ein dateiname (vollst�ngiger Pfad)
; value: eine Liste aller Ausdr�cke in der Datei.
  (let (res)
    (setq res nil)
    (if (and dateiname (probe-file dateiname))
        (setq res (multiple-value-list (ignore-errors (do* ((streamin (open dateiname))
                                                            (expr (read streamin nil 'eof) (read streamin nil 'eof))
                                                            (exprs nil))
                                                           ((equal expr 'eof) (close streamin) (nreverse exprs))
                                                        (setq exprs (cons expr exprs)))))))
    (cond ((= 1 (length res)) (first res))
          ((and (= 2 (length res)) (not (first res)))
           (setq inhalt (datei-lesen-in-einen-string dateiname))
           (Setq exprs (read-alle-from-string-do inhalt)))
          (t nil))))

(defun load-mit-read (dateiname)
  (mapc 'eval (datei-lesen dateiname)))

(defun datei-lesen-zeilen (dateiname) 
; input: ein dateiname (vollst�ngiger Pfad)
; value: eine Liste mit Strings, je Zeile ein String.
  (if (probe-file dateiname)
      (do* ((streamin (open dateiname))
            (expr (read-line streamin nil 'eof) (read-line streamin nil 'eof))
            (exprs nil))
           ((equal expr 'eof) (close streamin) (nreverse exprs))
        (setq exprs (cons expr exprs)))
    nil))


(defun datei-lesen-in-einen-string (dateiname &key (max-anz nil))
; input: ein dateiname (vollst�ngiger Pfad)
; value: ein String.
  (if (probe-file dateiname)
      (do* ((streamin (open dateiname)) (n 0 (+ n 1))
            (expr (read-char streamin nil 'eof) (read-char streamin nil 'eof))
            (exprs nil))
           ((or (equal expr 'eof) (and max-anz (> n max-anz)))
            (close streamin) (if exprs (map 'string 'identity (nreverse exprs)) " "))
        (setq exprs (cons (if (member (char-code expr) '(10 13)) #\space expr) exprs)))
    " "))

(defun datei-lesen-in-einen-string-mit-zeilenumbruch (dateiname &key anz format) ; (setq dateiname txt-datei)
  (if (probe-file dateiname)
      (do* ((streamin (if format (open dateiname :external-format format) (open  dateiname)))
            (n 0 (+ n 1))
            (expr (read-char streamin nil 'eof) (read-char streamin nil 'eof))
            (exprs nil))
           ((or (equal expr 'eof) (and anz (> n anz))) (close streamin) (if exprs (map 'string 'identity (nreverse exprs)) " "))
        (setq exprs (cons   expr exprs)))
    " "))

(defun datei-lesen-in-einen-string-mit-nl (dateiname &key format) 
  (datei-lesen-in-einen-string-mit-zeilenumbruch dateiname :format format))

(defun datei-lesen-in-string-bis-ende (dateiname  anz) 
  (if (probe-file dateiname)
      (do* ((streamin (open dateiname)) (n 0 (+ n 1)) 
            (expr (read-char streamin nil 'eof) (read-char streamin nil 'eof))
            (exprs nil) (exprs2 nil))
           ((equal expr 'eof) (close streamin) (setq exprs (nconc exprs exprs2))
            (if exprs (map 'string 'identity (nreverse exprs)) ""))
        (cond ((> n anz) (setq exprs2 exprs) (setq exprs nil) (setq n 0)))
        (setq exprs (cons   expr exprs)))
    ""))


(defun datei-schreiben (dateiname exprs &key (nil-error t) (schreib-test nil) (open-error t))
; input: ein dateiname (vollst�ngiger Pfad)
;        eine Liste von Ausdr�cke.
; effekt: ausdr�cke werden in datei geschrieben
  (let (outstream datei-temp)
   ;  (cond ((and nil-error (not exprs)) (break "keine exprs in datei-schreiben")))
    ; (setq datei-temp "E:/test.txt")
    (setq datei-temp (concatenate 'string dateiname "#-#"))    
    (setq outstream (ignore-errors (open datei-temp :direction :output :if-exists :supersede)))
    (cond (outstream
           (mapc (lambda (x) (print x outstream) (terpri outstream)) exprs)
           (close outstream)
           (cond ((and exprs (or (not (probe-file datei-temp)) (= (system:file-size datei-temp) 0)))
                  (break (concatenate 'string "fehler datei-schreiben " dateiname)))
                 (schreib-test (if (not (equal exprs (datei-lesen datei-temp))) 
                                   (break (concatenate 'string "fehler inhalt datei-schreiben " dateiname))))
                 ((probe-file dateiname) (delete-file dateiname)))
           (rename-file-sleep datei-temp dateiname)
           t)
          (open-error (break (concatenate 'string "fehler open datei-schreiben " dateiname))))))

(defun liste-schreiben-in-string (liste &key trenn-string)
  (let (sliste)
    (setq sliste (mapcar (lambda (x) (cs (prin1-to-string x) (or trenn-string (map 'string 'code-char '(10))))) liste))
    (apply 'concatenate 'string sliste)))

(defun datei-schreiben-string (dateiname string )
  (let (outstream)
   ;  (cond ((and nil-error (not exprs)) (break "keine exprs in datei-schreiben")))
    (setq outstream (open dateiname :direction :output :if-exists :supersede))
      (princ string outstream) (terpri outstream)
    (close outstream)))


(defun datei-schreiben-add (dateiname exprs ) 
; input: ein dateiname (vollst�ngiger Pfad)
;        eine Liste von Ausdr�cke.
; effekt: ausdr�cke werden in datei hinzugef�gt
  (let (outstream)
    (setq outstream (open dateiname :direction :output :if-exists :append :if-does-not-exist :create))
    (mapc (lambda (x) (print x outstream) ) exprs)
    (terpri outstream)
    (close outstream)))

(defun datei-schreiben-add-liste (dateiname exprs ) 
  (let (outstream)
    (setq outstream (open dateiname :direction :output :if-exists :append :if-does-not-exist :create))
    (mapc (lambda (x) (print x outstream) ) exprs)
    (terpri outstream)
    (close outstream)))



(defun datei-schreiben-add-one (dateiname expr )
  (let (outstream)
    (setq outstream (open dateiname :direction :output :if-exists :append :if-does-not-exist :create))
    (terpri outstream)
    (format outstream "~s" expr)   
    (close outstream)))

(defun datei-schreiben-zeilen (dateiname zeilen) 
  (let (outstream)
    (setq outstream (open dateiname :direction :output :if-exists :supersede))
    (mapc (lambda (x) (princ x outstream) (terpri outstream)) zeilen)
    (close outstream)))

(defun datei-schreiben-zeilen-add (dateiname zeilen) 
  (let (outstream)
    (setq outstream (open dateiname :direction :output :if-exists :append :if-does-not-exist :create))
    (terpri outstream)
    (mapc (lambda (x) (princ x outstream) (terpri outstream)) zeilen)
    (close outstream)))

(defun datei-lesen-zeilen-rev (dateiname)   
  (do* ((streamin (open dateiname))
        (expr (read-line streamin nil 'eof) (read-line streamin nil 'eof))
        (exprs nil))
       ((equal expr 'eof) (close streamin)   exprs)
    (setq exprs (cons expr exprs))))

(defun datei-schreiben-utf8 (dateiname exprs) 
  (let (outstream)
    (setq outstream (open  dateiname :direction :output :if-exists :supersede :external-format :utf-8
                           :element-type 'simple-char))
    (mapc (lambda (x) (pprint x outstream) (terpri outstream)) exprs)
    (close outstream)))

(defun datei-schreiben-zeilen-utf8 (dateiname zeilen) 
  (let (outstream)
    (setq outstream (open  dateiname :direction :output :if-exists :supersede :external-format :utf-8
                           :element-type 'simple-char))
    (mapc (lambda (x) (princ x outstream) (terpri outstream)) zeilen)
    (close outstream)))

(defun datei-lesen-utf8 (dateiname) 
  (if (probe-file dateiname)
      (do* ((streamin (open  dateiname :EXTERNAL-FORMAT :UTF-8 :element-type 'simple-char))
            (expr (read streamin nil 'eof) (read streamin nil 'eof))
            (exprs nil))
           ((equal expr 'eof) (close streamin) (nreverse exprs))
        (setq exprs (cons expr exprs)))
    nil))

(defun datei-lesen-zeilen-utf8 (dateiname) 
  (if (probe-file dateiname)
      (do* ((streamin (open  dateiname :EXTERNAL-FORMAT :UTF-8 :element-type 'simple-char))
            (expr (read-line streamin nil 'eof) (read-line streamin nil 'eof))
            (exprs nil))
           ((equal expr 'eof) (close streamin) (nreverse exprs))
        (setq exprs (cons expr exprs)))
    nil))

(defun datei-lesen-byte (dateiname &key anz ausschluss ausschluss-folge) 
  (if (probe-file dateiname)
      (do* ((streamin (open dateiname :element-type '(unsigned-byte 8)))
            (n 1 (+ n 1))
            (expr (read-byte streamin nil 'eof) (read-byte streamin nil 'eof))
            (alt 0)
            (exprs nil))
           ((or (equal expr 'eof) (and anz (> n anz))) (close streamin) (nreverse exprs))
        (cond ((and ausschluss (member expr ausschluss))) ; nichts tun
              ((and ausschluss-folge (member expr ausschluss-folge) (= expr alt))) ; nichts tun
              (t (setq exprs (cons expr exprs)))))
    nil))

(defun stream-lesen-byte (streamin &key anz)
  (do* ((n 1 (+ n 1))
        (expr (read-byte streamin nil 'eof) (read-byte streamin nil 'eof))
        (exprs nil))
       ((or (equal expr 'eof) (and anz (> n anz)))  (nreverse exprs))
        (setq exprs (cons expr exprs))))

(defun stream-lesen-char-code (streamin &key anz)
  (do* ((n 1 (+ n 1))
        (expr (read-char  streamin nil 'eof) (read-char streamin nil 'eof))
        (exprs nil))
       ((or (equal expr 'eof) (and anz (> n anz)))  (nreverse exprs))
        (setq exprs (cons (char-code expr) exprs))))

(defun datei-lesen-bytes (dateiname &key anz)   
  (do* ((streamin (open dateiname :element-type  '(unsigned-byte 8)))
        (n 0 (+ n 1))
        (expr (read-byte streamin nil 'eof) (read-byte streamin nil 'eof))
        (exprs nil) (exprs-liste nil))
       ((or (equal expr 'eof) (and anz (> n anz)))
        (close streamin) (cond (exprs-liste (nreverse (cons (nreverse exprs) exprs-liste)))
                               (t (nreverse exprs))))
    (cond ((> n 5000000) (setq exprs-liste (cons (nreverse exprs) exprs-liste))
           (setq exprs (cons expr nil)) (setq n 0))
          (t (setq exprs (cons expr exprs))))))

(defun datei-schreiben-bytes (dateiname exprs) 
  (let (outstream)
    (setq outstream (open dateiname :direction :output :if-exists :supersede :element-type  '(unsigned-byte 8)))
    (mapc (lambda (x) 
            (cond ((listp x)
                   (mapc (lambda (y) (write-byte y outstream) ) x))
                  (t (write-byte x outstream))))
          exprs)
    (close outstream)))

(defun datei-lesen-schreiben-bytes (in out &key start end)   
  (do* ((streamin (open in :element-type  '(unsigned-byte 8)))
        (streamout (open out :direction :output :if-exists :supersede :element-type  '(unsigned-byte 8)))
        (n 0 (+ n 1))
        (expr (read-byte streamin nil 'eof) (read-byte streamin nil 'eof))
         )
       ((or (equal expr 'eof) (and end (> n end)))
        (close streamin) (close streamout))
    (cond ((or (not start) (< start n)) (write-byte expr streamout)))))

(defvar DS**trenncodes '(9 10 13 32))

(defun DS++string-ohne-trennzeichen (str)
  (string-trim (map 'string 'code-char DS**trenncodes) str))

(defun string-zerlegen-in-stringliste-tz (string &key (trennzeichen #\space) (start 0))
  ; (setq string "asd.asd.asd." start 3 trennzeichen #\.)
   ; (setq STRING          "5 53 2749 2775 2327 2359 53" trennzeichen #\space start 0)
  (let (pos)
  (cond ((>= start (length string)) nil)
        ((setq pos (position trennzeichen string :start start))
         (cons (subseq string start pos) (string-zerlegen-in-stringliste-tz string :trennzeichen trennzeichen 
                                                                               :start (+ 1 pos))))
        (t (list (subseq string start ))))))

(defun string-zerlegen-in-stringliste  (string &key (trenn-code '(9 10 13 32)))
  ; (setq string "a ab ac  " trenn-code '(9 10 13 32))
  (if (numberp trenn-code) (setq trenn-code (list trenn-code)))
  (do* ((posa 0) (i 0 (+ 1 i)) (sliste nil) (neuer-string-p nil)
        (trennchars (mapcar 'code-char trenn-code)))
       ((<= (length string) i) 
        (cond (neuer-string-p (setq sliste (cons (subseq string posa i) sliste))))
        (nreverse sliste))
    (cond ((member (elt string i) trennchars)
           (cond (neuer-string-p (setq sliste (cons (subseq string posa i) sliste)) 
                                 (setq neuer-string-p nil))))
          (t (cond ((not neuer-string-p) (setq posa i) (setq neuer-string-p t)))))))

(defun string-zerlegen-in-stringliste-2  (string &key (trenn-code '(9 10 13 32)) (trim-blank-p t)) ; (setq string z trenn-code 59)
  ; (setq string "und Dr. Gr�nebergbeschlossen:Die Beschwerd" trenn-code '( 10 13 ))
  ; (map 'list 'char-code ",.;:-_")
  (if (numberp trenn-code) (setq trenn-code (list trenn-code)))
  (do* ((posa 0) (i 0 (+ 1 i)) (sliste nil) (neuer-string-p nil)
        (trennchars (mapcar 'code-char trenn-code)))
       ((<= (length string) i) 
        (cond (neuer-string-p (setq sliste (cons (if trim-blank-p (string-trim " " (subseq string posa i)) (subseq string posa i))
                                                 sliste))))
        (nreverse sliste))
    (cond ((member (elt string i) trennchars)
           (cond (neuer-string-p (setq sliste (cons (if trim-blank-p (string-trim " " (subseq string posa i)) (subseq string posa i)) 
                                                    sliste)) 
                                 (setq neuer-string-p nil))
                 (t (setq sliste (cons nil sliste)))))
          (t (cond ((not neuer-string-p) (setq posa i) (setq neuer-string-p t)))))))

(defun string-zerlegen-in-stringliste-mit-leerzeilen  (string &key (trenn-code '(9 10 13 32)) (trim-blank-p t))
  ; (setq string "und Dr. Gr�nebergbeschlossen:Die Beschwerd" trenn-code '( 10 13 ))
  (if (numberp trenn-code) (setq trenn-code (list trenn-code)))
  (do* ((posa 0) (i 0 (+ 1 i)) (sliste nil) (neuer-string-p nil)
        (trennchars (mapcar 'code-char trenn-code)))
       ((<= (length string) i) 
        (cond (neuer-string-p (setq sliste (cons (if trim-blank-p (string-trim " " (subseq string posa i)) (subseq string posa i))
                                                 sliste))))
        (nreverse sliste))
    (cond ((member (elt string i) trennchars)
           (cond (neuer-string-p (setq sliste (cons (if trim-blank-p (string-trim " " (subseq string posa i)) (subseq string posa i)) 
                                                    sliste)) 
                                 (setq neuer-string-p nil))
                 (t (setq sliste (cons "" sliste)))))
          (t (cond ((not neuer-string-p) (setq posa i) (setq neuer-string-p t)))))))


; (LISTE-BILDE-UNTERLISTEN-anz-do '(1 2 3 4 5 6 7 7 8 9  1 1 11 11 1112 313 412) 5)
(DEFUN LISTE-BILDE-UNTERLISTEN-anz-do (LISTE anz)
  (COND ((NOT LISTE) LISTE)
        (T 
         (DO (STRUKT-LISTE (REST-LISTE LISTE (REST REST-LISTE)) (nr -1))
             ((NOT REST-LISTE)
              (RPLACA STRUKT-LISTE (NREVERSE (FIRST STRUKT-LISTE)))
              (NREVERSE STRUKT-LISTE))
           (setq nr (+ 1 nr))
           (COND ((NOT STRUKT-LISTE)
                  (setq STRUKT-LISTE (LIST (LIST (FIRST REST-LISTE)))))
                 ((< nr anz)
                  (RPLACA STRUKT-LISTE
                          (CONS (FIRST REST-LISTE)
                                (FIRST STRUKT-LISTE))))
                 (T (setq nr 0)
                  (RPLACA STRUKT-LISTE (NREVERSE (FIRST STRUKT-LISTE)))
                  (setq STRUKT-LISTE
                        (CONS (LIST (FIRST REST-LISTE))
                              STRUKT-LISTE))))))))

(DEFUN LISTE-BILDE-UNTERLISTEN-do (LISTE PRED)
  (COND ((NOT LISTE) LISTE)
        (T
         (DO (STRUKT-LISTE (REST-LISTE LISTE (REST REST-LISTE)))
             ((NOT REST-LISTE)
              (RPLACA STRUKT-LISTE (NREVERSE (FIRST STRUKT-LISTE)))
              (NREVERSE STRUKT-LISTE))
           (COND ((NOT STRUKT-LISTE)
                  (setq STRUKT-LISTE (LIST (LIST (FIRST REST-LISTE)))))
                 ((FUNCALL PRED (CAAR STRUKT-LISTE) (FIRST REST-LISTE))
                  (RPLACA STRUKT-LISTE
                          (CONS (FIRST REST-LISTE)
                                (FIRST STRUKT-LISTE))))
                 (T
                  (RPLACA STRUKT-LISTE (NREVERSE (FIRST STRUKT-LISTE)))
                  (setq STRUKT-LISTE
                        (CONS (LIST (FIRST REST-LISTE))
                              STRUKT-LISTE))))))))
                 
(defun liste-bilde-unterlisten (liste pred)
  (liste-bilde-unterlisten-n (copy-list liste) pred))

(defun liste-bilde-unterlisten-n (liste pred)
  ; destruktive Version von liste-bilde-unterlisten 
  (do (alte-liste (strukt-liste (list liste)) (restliste  liste ))
      ((not (rest restliste)) strukt-liste)   ; (setq restliste '(4 2 5)) (setq strukt-liste (list restliste)) (setq pred '<)
    (cond ((funcall pred (first restliste) (second restliste))
           (setq restliste (rest restliste)))
          (t (setq alte-liste restliste)
             (setq restliste (rest restliste))
             (rplacd alte-liste nil)
             (setq strukt-liste (nconc strukt-liste (list restliste)))))))

          
(defun alist-set-value (alist key wert)  ; (setq alist '((a) (b 3) (c e)) key 'a wert nil)
  (let (ulist)
    (setq ulist (assoc key alist :test 'equal))
    (cond ((= 1 (length ulist)) (nconc ulist (list wert)))
          (ulist (rplaca (rest ulist) wert))
          (t (setq alist (nconc alist (list (list key wert))))))
    alist))

(defun alist-add-value (alist key wert &key (nur-neue-p t))
  (let (ulist)
    (setq ulist (assoc key alist :test 'equal))
    (cond ((and ulist nur-neue-p (member wert (second ulist) :test 'equal)) t)
          ((= 1 (length ulist)) (nconc ulist (list wert)))
          (ulist (rplaca (rest ulist) (nconc (second ulist) (list wert))))
          (t (setq alist (nconc alist (list (list key (list wert)))))))
    alist))

(defun alist-hat-duplikate (alist)
  (cond ((not alist) nil)
        ((assoc (caar alist) (rest alist)) t)
        (t (alist-hat-duplikate (rest alist)))))
  
(defun alist-delete-value (alist key wert)
  (let (ulist)
    (setq ulist (assoc key alist :test 'equal))
    (if ulist (rplaca (rest ulist) (delete wert (second ulist) :test 'equal)))
    alist))

(defun alist-delete-all-values (alist key  )
  (let (ulist)
    (setq ulist (assoc key alist :test 'equal))
    (if ulist (rplacd  ulist nil))
    alist)) 

(defun alist-set-value-neue-seite (alist)
  (let (ulist)
    (setq ulist (assoc 'mehrseitigkeit alist :test 'equal))
    (if ulist (rplaca (rest ulist) "neuer Beleg")
      (nconc alist (list (list 'mehrseitigkeit "neuer Beleg"))))
    alist))
 
(defun alist-set-value-folgeseite (alist)
  (let (ulist)
    (setq ulist (assoc 'mehrseitigkeit alist :test 'equal))
    (if ulist (rplaca (rest ulist) "Folgeseite")
      (nconc alist (list (list 'mehrseitigkeit "Folgeseite"))))
    alist))
       
(defun alist-value (alist key)
  (second (assoc key alist :test 'equal)))

(defun alist-value-list (alist key)
  (let (wert)
    (cond ((and (setq wert (second (assoc key alist :test 'equal)))
                (not (consp wert)))
           (rest (assoc key alist :test 'equal)))
          (t wert))))

(defun assoclist (key liste)
  (find-if (lambda (x) (and (consp x) (equal (first x) key))) liste)
  )

(defun  assocsecond (schluessel alist )  "wie assoc mit zus�tzlichem second"
  (second (assoc schluessel alist)))

(defun assoccdr (schluessel alist )  "wie assoc mit zus�tzlichem cdr"
  (cdr (assoc schluessel alist)))
           
(defun deep-member (a liste) 
  (cond ((not liste) nil)
        ((equal a (first liste)) t)
        ((listp (first liste)) (or (deep-member a (first liste))
                                   (deep-member a (rest liste))))
        (t (deep-member a (rest liste)))))

(defun search-alle (substring string &key (start 0) end)
  (do ((pos start) (res nil))
      ((or (not pos) (< (length string) pos)) (nreverse res))
    (setq pos (search substring string  :start2 pos :end2 end))
    (cond (pos (setq res (cons pos res))
               (setq pos (+ (length substring) pos))))
    ))

(defun insert-pos-n (liste elem pos)
  (cond ((= 0 pos) (cons elem liste))
        ((>= pos (length liste)) (nconc liste (list elem)))
        (t (let (rest) (setq rest (nthcdr (- pos 1) liste))
             (rplacd rest (cons elem (rest rest))))
           liste)))

; (insert-pos-n '(1 2 3 4 5) 0 7)

(defun liste-make-string  (liste &key (trennstring " "))
  (strings-make-string (mapcar (lambda (x) (cond ((stringp x) x) ((symbolp x) (symbol-name x)) (T (prin1-to-string x)))) liste)
                       :trennstring trennstring))

(defun strings-make-string-neu (stringliste &key (trennstring " "))
  (cond ((stringp stringliste) stringliste)
        ((= 0 (length stringliste)) "")
        ((= 1 (length stringliste)) (first stringliste))
        (t
         (do ((reststrings stringliste (rest reststrings)) (result ""))
             ((not reststrings) result)
           (setq result (concatenate 'string result " " (first reststrings)))))))
       

(defun strings-make-string (stringliste &key (trennstring " "))
  (string-trim trennstring (strings-make-string-rek stringliste trennstring)))

(defun strings-make-string-rek (stringliste  trennstring )
  (cond ((stringp stringliste) stringliste)
        ((= 0 (length stringliste)) "")
        ((= 1 (length stringliste)) (first stringliste))
        ((> (length stringliste) 254)
         (concatenate 'string (strings-make-string-rek (subseq stringliste 0 250) trennstring) 
                      (strings-make-string-rek (subseq stringliste  250) trennstring)))
        (t (apply 'concatenate 'string 
                  (mapcar (lambda (x) (cs x  trennstring )) stringliste)))))

(defun strings-make-string-mit-nl (stringliste)
  (cond ((stringp stringliste) stringliste)
        ((= 0 (length stringliste)) "")
        ((= 1 (length stringliste)) (first stringliste))
        ((> (length stringliste) 254)
         (concatenate 'string (strings-make-string-mit-nl (subseq stringliste 0 250)) 
                      (strings-make-string-mit-nl (subseq stringliste  250))))
        (t (concatenate 'string (first stringliste) (make-sequence 'string 1 :initial-element (code-char 10)) 
                        (strings-make-string-mit-nl (rest stringliste))))))


(defun symbolliste-make-string (symbolliste)
  (if symbolliste (string-trim "()" (prin1-to-string symbolliste)) ""))

; (symbolliste-make-string '())


(defun ordnername (datei)
  (subseq datei (+ 1 (position #\\ datei :from-end t :end (position #\\ datei :from-end t)))
          (position #\\ datei :from-end t)))

(defun ordnerpfad-get-ordnername (ordner)
  (setq ordner (string-trim "/\\" ordner))
  (subseq ordner (+ 1 (max (or (position #\\ ordner :from-end t) -1)  (or (position #\/ ordner :from-end t) -1)))))

(defun ordnerpfad-get-ordnerliste (ordner)  ; (setq ordner  "K:\\MUSIK-MP3-ADD")
  (let (pos)
  (setq ordner (string-trim "/\\" ordner))
  (setq pos (position-if (lambda (x) (member x '(#\\ #\/))) ordner :from-end t))
  (cond ((not pos) (list  ordner))
        (t (nconc (ordnerpfad-get-ordnerliste (subseq ordner 0 pos))
                  (list (string-trim "/\\" (subseq ordner  pos))))))))
        
(defun opt (liste pred)
  (let (wert)
    (setq wert (first liste))
    (mapc (lambda (x) (if (funcall pred x wert) (setq wert x))) liste)
    wert))

; (opt '(4 2 4 23 123 4) '>)
 
(defun first< (e1 e2) (< (first e1) (first e2)))
(defun first> (e1 e2) (> (first e1) (first e2)))
(defun second< (e1 e2) (< (second e1) (second e2)))
(defun second> (e1 e2) (> (second e1) (second e2)))

(defun liste< (l1 l2) 
  (cond ((or (not l1) (not l2)) nil)
        ((< (first l1) (first l2)) t)
        ((> (first l1) (first l2)) nil)
        (t (liste< (rest l1) (rest l2)))))

(defun liste> (l1 l2) ; echt >
  (cond ((or (not l1) (not l2)) nil)
        ((> (first l1) (first l2)) t)
        ((< (first l1) (first l2)) nil)
        (t (liste> (rest l1) (rest l2)))))

(defun list> (l1 l2) ; entspricht >=
  (or (and (not l1) (not l2))
      (and l1 l2 (listp l1) (listp l2) 
           (numberp (first l1)) (numberp (first l2))
           (or (> (first l1) (first l2))
               (and (= (first l1) (first l2))
                    (list> (rest l1) (rest l2)))))))


(defun read-alle-from-string (key &optional (start 0))
  ; (setq start 15)  (setq key "\"temp-kiv.lisp\"   NIL   NIL   NIL   ") (elt  key 15)
  (let (expr)
    (setq expr (multiple-value-list (ignore-errors-read  (read-from-string key nil 'eof :start start))))
    (cond ((not (first expr)) (read-alle-from-string key (+ start 1)))
          ((equal (first expr) 'eof) nil)
          ((and (consp (first expr)) (equal (caar expr) 'quote)) (cons (caar expr) (read-alle-from-string key (second expr))))
          ((consp (first expr)) (append (first expr) (read-alle-from-string key (second expr))))
          (t (cons (first expr) (read-alle-from-string key (second expr)))))))

(defun read-alle-from-string-do (string)
  (do ((exprs nil) expr (start 0))
      (nil)
     ; (setq expr (multiple-value-list (ignore-errors      (read-from-string string nil 'eof :start start))))
    (setq expr (multiple-value-list (ignore-errors-read (read-from-string string nil 'eof :start start))))
    (cond ((not (first expr)) (setq start (+ start 1)))
          ((equal (first expr) 'eof) (return (nreverse exprs)))
          (t (setq exprs (cons (first expr) exprs))
             (setq start (second expr))))))


(defun read-from-string-make-list (string)
  (read-from-string (concatenate 'string "(" string ")")))


; *********************************************************************************************************
; ****************************************     Datei-Datum setzen     *************************************
; *********************************************************************************************************

(defun datei-datum-setzen (datei datum &key touch) ; (break "datum")
  (sys:call-system (concatenate 'string
                                (if touch touch
                                  (cs *laufwerk* ":\\software-pc\\open-source-tools\\Datei-Datum-setzen\\touch.exe"))
                                " -d" datum " " "\"" datei "\"")))

(defun datei-zeit-setzen (datei zeit  &key touch) ;  (break "zeit")
  (sys:call-system (concatenate 'string
                                (if touch touch
                                  (cs *laufwerk* ":\\software-pc\\open-source-tools\\Datei-Datum-setzen\\touch.exe"))
                                " -d" zeit " " "\"" datei "\"")))

;  (datei-datum-setzen "C:/maus-error2.txt" "05/03/08")
;  (datei-zeit-setzen "C:/maus-error2.txt" "17:00:00")
; (setq datei "C:/info-bildschirm-aufloesung.rtf")
; (setq zeit (FILE-WRITE-DATE datei))
; (setq zeitliste (multiple-value-list (DECODE-UNIVERSAL-TIME   zeit )))
         
(defvar *datei-zeit-test-p* nil)
(setq *datei-zeit-test-p* t)
; (setq *datei-zeit-test-p* nil)

(defvar *datei-zeit-funktion* 'unitime-get-zeitliste1)

(defun datei-datum-unitime-setzen (datei zeit &key (wiederholung 1) (add 0) (zeit-test-p nil)) ; (setq datei neu zeit (FILE-WRITE-DATE td))
  ; (setq td datei tz zeit  tw wiederholung ta add tt zeit-test-p) (break "unitime")  ; (setq wiederholung 1 add 0 zeit-test-p nil)
  ; (setq datei out zeit (FILE-WRITE-DATE in))  (setq wiederholung 0 add 0) (setq wiederholung 1 add -3600)
  (let (zeitliste datum-string zeit-string datum-exe datum-setzen unizeit)
    (setq datum-setzen (multiple-value-list  (ignore-errors (system:SET-FILE-DATEs  out :modification  zeit))))
    (cond ((and (= 2 (length datum-setzen)) (not (first datum-setzen)))
           (setq unizeit (+ add zeit))
           (setq zeitliste (funcall *datei-zeit-funktion* unizeit))
           (setq datum-string (cs (if (< (fifth zeitliste) 10) "0" "") (prin1-to-string (fifth zeitliste)) "/"
                                  (if (< (fourth zeitliste) 10) "0" "") (prin1-to-string (fourth zeitliste)) "/"
                                  (subseq (prin1-to-string (sixth zeitliste)) 2)))
           (Setq zeit-string (cs (if (< (third zeitliste) 10) "0" "") (prin1-to-string (third zeitliste)) ":"
                                 (if (< (second zeitliste) 10) "0" "") (prin1-to-string (second zeitliste)) ":"
                                 (if (< (first zeitliste) 10) "0" "") (prin1-to-string (first zeitliste)) ))
           (Setq datum-exe (cs ds**source-pfad "touch.exe"))
           ; (setq zeit-string "02:04:05")
           (cond ((probe-file datum-exe)
                  (datei-datum-setzen datei datum-string :touch datum-exe)
                  (datei-zeit-setzen datei zeit-string :touch datum-exe)
                  (setq zeitneu (FILE-WRITE-DATE datei))
                  (cond ((and *datei-zeit-test-p* (not (unizeit= zeitneu unizeit)))
                         (datei-zeit-funktion-wechseln)
                         (setq zeitliste (funcall *datei-zeit-funktion* unizeit))
                         (setq datum-string (cs (if (< (fifth zeitliste) 10) "0" "") (prin1-to-string (fifth zeitliste)) "/"
                                                (if (< (fourth zeitliste) 10) "0" "") (prin1-to-string (fourth zeitliste)) "/"
                                                (subseq (prin1-to-string (sixth zeitliste)) 2)))
                         (Setq zeit-string (cs (if (< (third zeitliste) 10) "0" "") (prin1-to-string (third zeitliste)) ":"
                                               (if (< (second zeitliste) 10) "0" "") (prin1-to-string (second zeitliste)) ":"
                                               (if (< (first zeitliste) 10) "0" "") (prin1-to-string (first zeitliste)) ))
                         (datei-datum-setzen datei datum-string :touch datum-exe)
                         (datei-zeit-setzen datei zeit-string :touch datum-exe)
                         (setq zeitneu (FILE-WRITE-DATE datei))
                         ))
                  (cond ((unizeit= zeit zeitneu) t)
                        ((and (< wiederholung 5) (unizeit= 3600 (- zeit zeitneu)))
                         (datei-datum-unitime-setzen datei zeit :add (* wiederholung 3600) :wiederholung (+ 1 wiederholung)))
                        ((and (< wiederholung 5) (unizeit= 3600 (- zeitneu zeit)))
                         (datei-datum-unitime-setzen datei zeit :add (* wiederholung -3600) :wiederholung (+ 1 wiederholung)))
                        ((unizeit= 3600 (- zeitneu zeit)) t)
                        ((unizeit= 7200 (- zeitneu zeit)) t)
                        (t  nil) ; (break "Datum setzen ist inkonsistent.")
                        ))
                 (t (break (cs datum-exe " fehlt, Datum kann nicht gesetzt werden."))
                    nil)))
          (t t))))

;  (trace datei-datum-unitime-setzen)
    
(defun unizeit= (x y) (or (= x y) (< (abs (- x y)) 2)))

(defun unitime-get-zeitliste1 (zeit)  ; (setq zeit 3383928678)
  (multiple-value-list (DECODE-UNIVERSAL-TIME zeit)))

(defun unitime-get-zeitliste2 (zeit)  ; (setq zeit 3383928678)
  (let (liste)
    (setq liste (multiple-value-list (DECODE-UNIVERSAL-TIME zeit)))
    (cond ((eighth liste) (multiple-value-list (DECODE-UNIVERSAL-TIME (- zeit 3600))))
          (t liste))))

(defun datei-zeit-funktion-wechseln ()
  (cond ((equal *datei-zeit-funktion* 'unitime-get-zeitliste1)
         (setq *datei-zeit-funktion* 'unitime-get-zeitliste2))
        ((equal *datei-zeit-funktion* 'unitime-get-zeitliste2)
         (setq *datei-zeit-funktion* 'unitime-get-zeitliste1))))



; *********************************************************************************************************
; ****************************************       *************************************
; *********************************************************************************************************


(defun datei-waehlen (&rest args)
  (let (antwort)
    (setq antwort (namestring   (capi:prompt-for-file
                   "Datei ausw�hlen"
                   :pathname "")))
    ))

; (meldung-ja-nein "test" "meldung" "" #'(lambda (&rest args)) #'(lambda (&rest args) ))

(defun meldung-ja-nein (zeile1 zeile2 zeile3 f-ja f-nein &key (ja-text "Ja") (nein-text "Nein"))
  (EPA++meldung-ja-nein-mit-warten zeile1 zeile2 zeile3 f-ja f-nein :ja-text ja-text :nein-text nein-text))

(defun EPA++meldung-ja-nein-mit-warten (zeile1 zeile2 zeile3 f-ja f-nein &key (ja-text "Ja") (nein-text "Nein"))
(capi:display-dialog
 (capi:make-container
  (make-instance 'capi:column-layout
                 :x-adjust :center
                 :background :yellow
                 :min-width 600;breite des fensters in pixel (muss unten auch angepasst werden)
                 :description
                 (list (make-instance  'capi:title-pane  :text "")
                       (make-instance
                        'capi:title-pane
                        :text zeile1)  ;der text in dem feld
                       (make-instance
                        'capi:title-pane
                        :text zeile2)
                       (make-instance
                        'capi:title-pane
                        :text zeile3)
                       (make-instance  'capi:title-pane  :text "")
                       (make-instance 'capi:row-layout
                                      :description
                                      (list
                                       (make-instance
                                        'capi:push-button
                                        :text ja-text  ;text des linken buttons
                                        :callback-type :data
                                        :callback 
                                        #'(lambda (arg)   (funcall f-ja)
                                                      (capi:exit-dialog arg)))
                                        ;funktion die aufgerufen wird, bei klick auf en button
                                       ;***********anfang zweiter button
                                       (make-instance
                                        'capi:push-button
                                        :text nein-text   ;text des rechten buttons
                                        :callback-type :data
                                        :callback #'(lambda (arg)   (funcall f-nein)
                                                      (capi:exit-dialog arg)));funktion die aufgerufen wird, bei klick auf en button
                                       ;***********ende zweiter button (den zweiten button einfach l�schen zum entfernen)
                                       ))))
  :title "Mitteileung - Frage";das was oben in der blauen leiste steht
  :min-width 600;breite des fensters in pixel (muss oben auch angepasst werden)
  )))

; (meldung-ok "test" "" "test")

(defun meldung-ok (zeile1 zeile2 zeile3 &optional (zeile4 "") (zeile5 "")  (zeile6 "") &key (farbe :yellow))
  (EPA++meldung-ok-mit-warten zeile1 zeile2 zeile3 zeile4 zeile5 zeile6 :farbe farbe))

(defun EPA++meldung-ok-mit-warten (zeile1 zeile2 zeile3 &optional (zeile4 "") (zeile5 "")  (zeile6 "") &key (farbe :yellow))
(capi:display-dialog
 (capi:make-container
  (make-instance 'capi:column-layout
                 :x-adjust :center
                 :min-width 600  ;breite des fensters in pixel (muss unten auch angepasst werden)
                 :background farbe
                 ; :foreground :red
                 :description
                 (list (make-instance
                        'capi:title-pane
                        :text zeile1)  ;der text in dem feld
                       (make-instance
                        'capi:title-pane
                        :text zeile2)
                       (make-instance
                        'capi:title-pane
                        :text zeile3)
                       (make-instance
                        'capi:title-pane
                        :text zeile4)
                       (make-instance
                        'capi:title-pane
                        :text zeile5)
                       (make-instance
                        'capi:title-pane
                        :text zeile6)
                       (make-instance 'capi:row-layout
                                      :description
                                      (list
                                       (make-instance
                                        'capi:push-button
                                        :text "OK";text des linken buttons
                                        :callback-type :data
                                        :callback 
                                        #'(lambda (arg)  
                                                      (capi:exit-dialog arg)))
                                        ;funktion die aufgerufen wird, bei klick auf en button
                                       ))))
  :title "Mitteilung";das was oben in der blauen leiste steht
  :min-width 600;breite des fensters in pixel (muss oben auch angepasst werden)
  )))

