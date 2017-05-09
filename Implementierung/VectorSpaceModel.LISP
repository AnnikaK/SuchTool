(defvar *terms* (make-hash-table :test 'equalp)) ;key is term, value is tuple (vector-index document-frequency)
(defvar *special-characters* '(#\# #\' #\" #\\ #\/))
(defvar *vocabulary* '())



(defun add-to-vocabulary (doc-text)
   (mapcar (lambda (x)
            (setq words (process-string x))
            (mapcar (lambda (y) 
                      (cond ((not (member y *vocabulary*))
                             (setq *vocabulary* (cons y *vocabulary*)))))
                    words))
          doc-text))

(defun process-string (str) 
  (split-sequence:SPLIT-SEQUENCE #\Space  str :remove-empty-subseqs t))

(defun create-term-dictionary ()
  (let ((index 0) (term))
    (mapcar (lambda (x)
              ;(setq term (ignore-errors (read-from-string (delete-if (lambda (c) (member c *special-characters*)) x))))
                     (setq term x) ;TOSO: control check
                     (setf (gethash term *terms*) (make-term :index index :document-frequency 0))
                     (setq index (+ 1 index)))
              *vocabulary*)))
  
 

 
(defun create-document-vectors (free-text)
 (let ((size (length free-text)))
   (loop for x from 0 to size do
         (setq current-element (car free-text)) ;(docID text-content) , text-content ist liste mit line-strings
         (setq free-text (cdr free-text)) 
         (setq docID (first current-element))
         (setq text-content (second current-element))
         (cond ((not (equal nil docID))
                (setq doc-vector (make-hash-table))
                (mapcar (lambda (line)
                          (setq words (process-string line))
                          (mapcar (lambda (word)
                                    (setq current-term (gethash word *terms*))
                                    (cond ((not (equal nil current-term))
                                           (setq wortindex (term-index current-term))
                                           (if (gethash wortindex doc-vector) (setf (gethash wortindex doc-vector) (+ 1 (gethash wortindex doc-vector)))
                                             (setf (gethash wortindex doc-vector) 1))))) words)

                          ) text-content)

                (setf (document-document-vector (gethash docID *documents*)) doc-vector)  
               ))) (printDocuments) ;for testing!!!
))


;;;;----------------------------------------------------------------------------------------



(defun printDocuments () 
  (loop for key being the hash-key of *documents* do 
        (setq docID key)
        (printDict (document-document-vector (gethash docID *documents*)))))
        
(defun printTerms () 
  (loop for key being the hash-key of *terms* do 
        (print key)
        (setq value (gethash key *terms*))
        (print (term-index value))
        (print (term-document-frequency value))))

;(printTerms)

(printDict *documents*)

(printDict (document-document-vector (gethash 19 *documents*)))


(gethash "DFKI" *terms*)