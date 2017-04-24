(defvar *keyword-dict* (make-hash-table))

;;;;-----------------------------------------------------------------------------------------------------------------
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
; input: ein dateiname (vollstaengiger Pfad)
; value: eine Liste mit Strings, je Zeile ein String.
  (if (probe-file dateiname)
      (do* ((streamin (open dateiname))
            (currentLine (read-line streamin nil 'eof) (read-line streamin nil 'eof))
            (lineSet nil))
           ((equal currentLine'eof) (close streamin) (nreverse lineSet))
        (setq lineSet (cons currentLine lineSet)))
    nil))




;;;;---------------------------------------------------------------------------------------------------------

(defun split-name-content (string)
         (setq j (position #\Space string :start 0))
          (setq key-name (subseq string 0 j))
          (setq key-content (subseq string j (length string)))
          (list key-name key-content))



;form: dictionary mit keywords mit dem keyword-namen [symbol] als key und entries in der form (docID type content)
(defun process-keyword-list (keyword-list docID)
  (do* ((k-list keyword-list (cdr k-list)) (k (car k-list) (car k-list))) ;iterate over keyword list, k is the whole term!
       ((equal k-list nil) nil)
    (setq k (subseq k 1 (- (length k) 1))) ;dont want the brackets
    (setq current (split-name-content k)) 
    
    (setq key (read-from-string (car current)))
    (setq content (read-from-string (cdr current)))
    (setq type (type-of content))
    (setq entry (list docID  type content))

    (if (gethash key *keyword-dict*) (setf (gethash key *keyword-dict*) (cons entry (gethash key *keyword-dict*))) ;already existing
     (setf (gethash key *keyword-dict*) (list entry))))) ;new entry for keyword
 
            
         







;;;;---------------------------------------------------------------------------------------------------------

(defun test(l)
  (loop for x from 0 to (- (length l) 1) do
       (print (car l))
       (setq l (cdr l))))

(defun printDict (dict)
  (loop for key being the hash-key of dict do 
        (print key)(print (gethash key dict))))




(test (cdr (split (readFileInLines "C:\\Users\\kreme\\OneDrive\\PractisingLisp\\Lisp\\mail-beispiele-eingang\\mail-beispiele-eingang\\mail-588.ldd"))))


(process-keyword-list (car (split (readFileInLines "C:\\Users\\kreme\\OneDrive\\PractisingLisp\\Lisp\\mail-beispiele-eingang\\mail-beispiele-eingang\\mail-588.ldd"))) 1)


;(printDict *keyword-dict*)
