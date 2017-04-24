(defun split(expr-list)
  (let ((keywords-list '())(free-text '()))
    (mapcar (lambda (x)
              (if (isList x) (setq keywords-list (append (list x) keywords-list))
                (setq free-text (append (list x) free-text))))
            expr-list) 
 (list keywords-list free-text)))


(defun isList (x)
  (if ( > (length x) 0) 
      (if (and (equal (char x 0) '#\() (equal (char x (- (length x) 1)) '#\)))
          t nil)
    nil))


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
  (loop for x from 0 to (length l) do
       (print (car l))
       (setq l (cdr l))))



(test (cdr (split (readFileInLines "C:\\Users\\kreme\\OneDrive\\PractisingLisp\\Lisp\\mail-beispiele-eingang\\mail-beispiele-eingang\\mail-588.ldd"))))



