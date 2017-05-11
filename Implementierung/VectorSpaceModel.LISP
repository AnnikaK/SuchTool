(defvar *terms* (make-hash-table :test 'equalp)) ;key is term, value is tuple (vector-index document-frequency)
(defvar *special-characters* '(#\# #\' #\" #\/ #\\ #\; #\- #\( #\) #\[ #\]))
(defvar *vocabulary* '())
(defvar *query-vector*)
(defvar *score-list*)


(defstruct score :value :docID)


(defun add-to-vocabulary (doc-text)
  (let ((words))
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
    (mapcar (lambda (x)
              ;(setq term (ignore-errors (read-from-string (delete-if (lambda (c) (member c *special-characters*)) x))))
                     (setq term x) ;TOSO: control check
                     (setf (gethash term *terms*) (make-term :index index :idf 0))
                     (setq index (+ 1 index)))
              *vocabulary*)))
  
 

 
(defun create-document-vectors (free-text)
 (let ((size (length free-text)) (current-element) (docID) (text-content) (doc-vector) (alreadySet) (words) (current-term) (wordindex)) ;local variables
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
   (create-tf-idf-weighting) 
   ))





(defun transfrom-document-frequency-to-idf ()
  (let ((number-of-documents (hash-table-count *documents*)) (df) (idf))
    (loop for key being the hash-key of *terms* do
          (setq current-term (gethash key *terms*))
          (cond ((not (equal nil current-term))
                 (setq df(term-idf current-term))
                 (setq idf (log (/ (- number-of-documents df) df) 10))
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
                     (setq tf-idf (* (/ tf (+ 1 tf)) idf))
                     (setf (gethash wortindex doc-vector) tf-idf)))))))
              
        






(defun create-query-vector (user-query)
 (let ((words) (max 0) (tf-idf 0))
  (setq words (process-string user-query))
  (setq *query-vector* (make-hash-table)) ;always new initialization in case of multiple user queries
  (mapcar (lambda (word)  ;get term frequencies 
            (setq current-term (gethash word *terms*))
            (cond ((not (equal nil current-term))
                   (setq wortindex (term-index current-term))
                   (if (gethash wortindex *query-vector*) (setf (gethash wortindex *query-vector*) (+ 1 (gethash wortindex *query-vector*)))
                     (setf (gethash wortindex *query-vector*) 1))))) words)
  (loop for key being the hash-key of *query-vector* do  ;find maximum term-frequency in query 
        (setq tf (gethash key *query-vector*))
        (cond ((> tf max) (setq max tf))))
  (loop for key being the hash-key of *query-vector* do
        (setq current-term (gethash key *terms*))
        (cond ((not (equal nil current-term))  
               (setq idf (term-idf current-term))
               (setq tf-idf (* (/ (gethash key *query-vector*) max) idf)) ;FORMEL
               (setf (gethash key *query-vector*) tf-idf))))))









(defun cosinesSimilarity (document-vector)
 (let ((maxindex (- (hash-table-count *terms*) 1)) (cosine-similarity) (doc-tf-idf) (query-tf-idf) (tmp-result))
   (loop for x from 0 to maxindex do
         (if (gethash x document-vector) (setq doc-tf-idf (gethash x document-vector))
           (setq doc-tf-idf 0))
         (if (gethash x *query-vector*) (setq query-tf-idf (gethash x *query-vector*))
           (setq query-tf-idf 0))
         (setq tmp-result (/ (* doc-tf-idf query-tf-idf) (* (expt doc-tf-idf 2) (expt query-tf-idf 2))))
         (setq cosine-similarity (+ tmp-result  cosine-similarity)))
   cosine-similarity))


(defun calculateScores ()
  (let  ((doc-vector) (score) (score-value)(top-ten))
    (setq *score-list* '())
    (loop for docID being the hash-key of *documents* do
          (setq doc-vector (document-document-vector (gethash docID *documents*)))
          (setq score-value (cosine-similarity doc-vector))
          (setq score (make-score score-value docID))
          (cons score *score-list*))
    (setq *scores* (sort *scores* '> :key 'score-score-value))
    (setq top-five (subseq *scores* 0 (min 10 (length *scores*))))))



    

  



          
          
          
         
         
         
  
  












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



(create-query-vector "grillen im Mai") 