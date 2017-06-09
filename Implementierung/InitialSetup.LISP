(defvar *keyword-dict* (make-hash-table :test 'equalp)) ;key is term, value is posting list with doc-ID and type and content
(defvar *documents*) ;key is docID, value is tuple (docPath docVector)
(defvar *free-text* '())
(defvar *results* '())


(defstruct document path document-vector datum absender)
(defstruct term index idf)
(defstruct keyword-entry doc-id keyword-type content)






(defun readFileInLines (dateiname) 
  (if (probe-file dateiname)
      (do* ((streamin (open dateiname))
            (currentLine (read-line streamin nil 'eof) (read-line streamin nil 'eof))
            (lineSet nil))
           ((equal currentLine'eof) (close streamin) (nreverse lineSet))
        (setq lineSet (cons currentLine lineSet)))
    nil))




(defun split(expr-list)
  (let ((keywords-list '()) (free-text '()) (stack '()))
    (mapcar (lambda (x) 
              (if ( > (length x) 0) 
                  (cond ((and (equal (char x 0) '#\() (equal (char x (- (length x) 1)) '#\)))
                         (setq keywords-list (cons x keywords-list))) ;add keyword
                        ((and (equal (char x 0) '#\() (really-opening x))
                         (setq stack (cons x stack))) ;start of keyword over multiple lines, add to stack
                        ((> (length stack) 0) ;unfinished keyword on stack?
                         (if (and (equal (char x (- (length x) 1)) '#\)) (really-closing x)) 
                             (progn 
                               (setq keywords-list (cons (merge-multiple-line-keyword stack x) keywords-list))
                               (setq stack '())) ;keyword closing, merge stacked lines and add complete keyword
                           (setq stack (cons x stack)))) ;not closing yet, add to stack
                        (t (setq free-text (cons x free-text))))))  ;free text
            expr-list) 
    (list (nreverse keywords-list) (nreverse free-text))))




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
   


(defun split-name-content (str)
  (let ((key-name) (key-content) (j))
    (cond ((> (length str) 0)
           (setq j (position #\Space str :start 0))
           (setq key-name (subseq str 0 j))
           (setq key-content (subseq str j (length str)))
           (list key-name key-content)))))




(defun createDocumentDictionary(directory-name)
  (let ((id 0))
    (setq *documents* (make-hash-table))
    (mapcar (lambda (x) 
              (setf (gethash id *documents*) 
                    (make-document 
                     :path x 
                     :document-vector nil))
              (setq id (+ 1 id)))
            (directory directory-name))))



;form: dictionary mit keywords mit dem keyword-namen [symbol] als key und entries in der form (docID type content)
(defun process-keyword-list (keyword-list docID)
  (let ((current) (key) (entry) (type) (content))
    (do* ((k-list keyword-list (cdr k-list))
          (k (car k-list) (car k-list))) ;iterate over keyword list, k is the whole term!
         ((equal k-list nil) nil)
      (setq k (subseq k 1 (- (length k) 1))) ;dont want the brackets
      
      (setq current (split-name-content k)) 
      (setq key (read-from-string (first current)))
      (setq content (read-from-string (second current)))
      (setq type (type-of content))



      (cond ((equalp key 'datum) 
             (setf (document-datum (gethash docID *documents*)) content))
            ((equalp key 'absender) 
             (setf (document-absender (gethash docID *documents*)) content)))  
      (setq entry 
            (make-keyword-entry 
             :doc-id docID 
             :keyword-type type 
             :content content)) 
      (if (gethash key *keyword-dict*)
          (setf (gethash key *keyword-dict*) (cons entry (gethash key *keyword-dict*))) ;already existing
        (setf (gethash key *keyword-dict*) (list entry)))))) ;new entry for keyword



;returns all keys from  dictionary sorted alphabetically (+ freitext as special keyword)
(defun getKeys (dict)
  (let ((keylist '()))
    (loop for key being the hash-key of dict do 
          (setq keylist (cons key keylist)))
    (cons 'FREITEXT (sort keylist 'string<))))






;has to do all init steps, called when directory selected
(defun onSelected (directory-name)
  (let ((current-doc))
    (setq *results* '()) ;reset results because new directory
    (createDocumentDictionary directory-name)
    (loop for docID being the hash-key of *documents* do 
          (setq current-doc (split (readFileInLines (document-path (gethash docID *documents*)))))
          (process-keyword-list (first current-doc) docID)
          (setq *free-text* (cons (list docID (second current-doc)) *free-text*))
          (add-to-vocabulary (second current-doc)))
    (create-term-dictionary)
    (create-document-vectors *free-text*)
    (getKeys *keyword-dict*)))




