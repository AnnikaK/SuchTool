(defvar *terms* (make-hash-table)) ;key is term, value is tuple (vector-index document-frequency)
(defvar *special-characters* '(#\# #\' #\" #\/ #\\ #\; #\- #\( #\) #\[ #\]))
(defvar *vocabulary* '())
(defvar *query-vector*)
(defvar *query-vector-length*)
(defvar *document-vector-length*)
(defvar *score-list*)




(defun add-to-vocabulary (doc-text)
  (let ((words))
   (setq vocabulary '())
   (mapcar (lambda (x)
            (setq words (process-string x))
            (mapcar (lambda (y) 
                      (cond ((not (member y *vocabulary*))
                             (setq *vocabulary* (cons y *vocabulary*)))))
                    words))
          doc-text)))




(defun process-string (str) 
  (split-sequence:SPLIT-SEQUENCE #\Space  str :remove-empty-subseqs t))





(defun create-term-dictionary ()
  (let ((index 0) (term))
    (setq *terms* (make-hash-table :test 'equalp))
    (mapcar (lambda (x)
              ;(setq term (ignore-errors (read-from-string (delete-if (lambda (c) (member c *special-characters*)) x))))
                     (setq term x) 
                     (setf (gethash term *terms*) (make-term :index index :idf 0))
                     (setq index (+ 1 index)))
              *vocabulary*)))
  
 

 
(defun create-document-vectors (free-text)
 (let ((size (length free-text)) (current-element) (docID) (text-content) (doc-vector) (alreadySet) (words) (current-term) (wordindex)) 
   (loop for x from 0 to size do
         (setq current-element (car free-text)) ;(docID text-content) , text-content ist liste mit line-strings
         (setq free-text (cdr free-text)) 
         (setq docID (first current-element))
         (setq text-content (second current-element))
         (cond ((not (equal nil docID))
                (setq doc-vector (make-hash-table))
                (setq alreadySet (make-hash-table)) ;check if doc frequency already increased for terms
                (mapcar (lambda (line)
                          (setq words (process-string line))
                          (mapcar (lambda (word)
                                    (setq current-term (gethash word *terms*))
                                    (cond ((not (equal nil current-term))
                                           (setq wortindex (term-index current-term))
                                           (if (gethash wortindex doc-vector) (setf (gethash wortindex doc-vector) (+ 1 (gethash wortindex doc-vector))) ;increase term frequency
                                             (setf (gethash wortindex doc-vector) 1))
                                           (cond ((equal (gethash wortindex alreadySet) nil) ;increase doc-freuency (has to be transformed to idf later!!!)
                                                  (setf (gethash wortindex alreadySet) t)
                                                  (setf (term-idf current-term) (+ 1 (term-idf current-term)))))))) words))
                        text-content)
                (setf (document-document-vector (gethash docID *documents*)) doc-vector))))
   (transfrom-document-frequency-to-idf) 
   (create-tf-idf-weighting)))





(defun transfrom-document-frequency-to-idf ()
  (let ((number-of-documents (hash-table-count *documents*)) (df) (idf))
    (loop for key being the hash-key of *terms* do
          (setq current-term (gethash key *terms*))
          (cond ((not (equal nil current-term))
                 (setq df(term-idf current-term))
                 (setq idf (log (/ number-of-documents df) 10)) ;idf FORMEL S.108
                 (setf (term-idf current-term) idf))))))
                 
                 




(defun create-tf-idf-weighting ()
  (let ((document) (tf))
    (loop for docID being the hash-key of *documents* do
        (setq document (gethash docID *documents*))
        (setq doc-vector (document-document-vector document))
        (loop for wortindex being the hash-key of doc-vector do
              (setq tf (gethash wortindex doc-vector))
              (setq current-term (gethash wortindex *terms*))
              (cond ((not (equal nil current-term))
                     (setq idf (term-idf current-term))
                     (setq tf-idf (* tf idf)) 
                     (setf (gethash wortindex doc-vector) tf-idf)))))))
              
        






(defun create-query-vector (user-query)
 (let ((words) (current-term) (wortindex) (idf 0) (tf-idf 0))
   (setq words (process-string user-query))
   (setq *query-vector* (make-hash-table)) ;always new initialization in case of multiple user queries
   (mapcar (lambda (word)  ;get term frequencies 
             (setq current-term (gethash word *terms*))
             (cond ((not (equal nil current-term))
                    (setq wortindex (term-index current-term))
                    (if (gethash wortindex *query-vector*) (setf (gethash wortindex *query-vector*) (+ 1 (gethash wortindex *query-vector*)))
                      (setf (gethash wortindex *query-vector*) 1))))) words)
   (loop for key being the hash-key of *query-vector* do
         (setq current-term (gethash key *terms*))
         (cond ((not (equal nil current-term))  
                (setq idf (term-idf current-term))
                (setq tf-idf (* (gethash key *query-vector*) idf))
                (setf (gethash key *query-vector*) tf-idf))))))




;normalize vector
(defun vector-length (vector)
  (let ((laenge 0))
    (loop for index being the hash-key of vector do
          (setq laenge (+ laenge (expt (gethash index vector) 2))))
    (sqrt laenge)))
        
        

(defun cosine-similarity (document-vector)
 (let ((dot-product 0) (weight-doc) (weight-query) (doc-vector-length (vector-length document-vector)) (res))
   (cond ((not (equal 0.0 *query-vector-length*)) 
          (loop for x being the hash-key of *query-vector* do
                       (setq weight-query (gethash x *query-vector*))
                       (if (gethash x document-vector) (setq weight-doc (gethash x document-vector)) (setq weight-doc 0))
                       (setq dot-product (+ dot-product (* weight-doc weight-query))))
            (/ dot-product (* doc-vector-length *query-vector-length*)))
         (t 0))))




(defun calculateScores ()
  (let  ((doc-vector) (score-value))
    (setq *score-list* '())
    (loop for docID being the hash-key of *documents* do
          (setq doc-vector (document-document-vector (gethash docID *documents*)))
          (setq score-value (cosine-similarity doc-vector))
          (setq *score-list* (cons (list score-value docID) *score-list*))
    (setq *score-list* (sort *score-list* '> :key 'car)))))


(defun calculate-top-results (score-list)
  (let ((top-ten) (top '()))
    (setq top-ten (subseq *score-list* 0 (min 10 (length *score-list*))))
    (mapcar (lambda (x) (if (> (car x) 0) (setq top (cons x top)))) top-ten)
    top))



(defun on-free-text-query-submitted (query)
 (let ((top))
  (create-query-vector query)
  (setq *query-vector-length* (vector-length *query-vector*))
  (calculateScores)
  (setq top (calculate-top-results *score-list*))
))










        
       

;;;;----------------------------------------------------------------------------------------TESTING 



(defun printDocuments () 
  (loop for key being the hash-key of *documents* do 
        (setq docID key)
        (printDict (document-document-vector (gethash docID *documents*)))))
        
(defun printTerms () 
  (loop for key being the hash-key of *terms* do 
        (print key)
        (setq value (gethash key *terms*))
        (print (term-index value))
        (print (term-idf value))))

;(printTerms)

;(printDict *documents*)
;(printDict *query-vector*)


;(printDict (document-document-vector (gethash 2 *documents*)))


;(gethash "DFKI" *terms*)



;(create-query-vector "grillen im Mai") 
