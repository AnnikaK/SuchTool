
(defun split(expr-list)
  (setq keywords-list '())
  (setq free-text '())
  (setq stack '())
    (mapcar (lambda (x) 
      (if ( > (length x) 0) 
      (cond ((and (equal (char x 0) '#\() (equal (char x (- (length x) 1)) '#\))) (setq keywords-list (cons x keywords-list))) ;add keyword
            ((and (equal (char x 0) '#\() (really-opening x)) (setq stack (cons x stack))) ;start of keyword over multiple lines, add to stack
            ((> (length stack) 0) ;unfinished keyword on stack?
             (if (and (equal (char x (- (length x) 1)) '#\)) (really-closing x)) (progn (setq keywords-list (cons (merge-multiple-line-keyword stack x) keywords-list)) (setq stack '())) ;keyword closing, merge stacked lines and add complete keyword
               (setq stack (cons x stack)))) ;not closing yet, add to stack
            (t (setq free-text (cons x free-text))))))  ;free text
            expr-list) 
 (list (nreverse keywords-list) (nreverse free-text)))


(defun merge-multiple-line-keyword (stack curr)
  (let ((expr curr))
    (mapcar (lambda (x)
          (setq expr (concatenate 'string x expr))) stack)
    expr))


;check if ")" is left over 
(defun really-closing (x) 
  (let ((numberOpening (count '#\( x :test #'equal)) (numberClosing (count '#\) x :test #'equal)))
    (equal 1 (- numberClosing numberOpening))))

;check if "(" is left over 
(defun really-opening (x) 
  (let ((numberOpening (count '#\( x :test #'equal)) (numberClosing (count '#\) x :test #'equal)))
    (equal 1 (- numberOpening numberClosing))))
    
    




(defun readFileInLines (dateiname) 
; input: ein dateiname (vollstängiger Pfad)
; value: eine Liste mit Strings, je Zeile ein String.
  (if (probe-file dateiname)
      (do* ((streamin (open dateiname))
            (currentLine (read-line streamin nil 'eof) (read-line streamin nil 'eof))
            (lineSet nil))
           ((equal currentLine'eof) (close streamin) (nreverse lineSet))
        (setq lineSet (cons currentLine lineSet)))
    nil))







(defun test(l)
  (loop for x from 0 to (- (length l) 1) do
       (print (car l))
       (setq l (cdr l))))



(test (cdr (split (readFileInLines "C:\\Users\\kreme\\OneDrive\\PractisingLisp\\Lisp\\mail-beispiele-eingang\\mail-beispiele-eingang\\mail-588.ldd"))))



