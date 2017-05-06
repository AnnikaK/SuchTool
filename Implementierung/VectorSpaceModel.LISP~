(defvar *terms* (make-hash-table)) ;key is term, value is tuple (vector-index document-frequency)
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
                     (setf (gethash term *terms*) index)
                     (setq index (+ 1 index)))
              *vocabulary*)))
  
  
  
(defun create-document-vectors (free-text)
 (let ((size (length free-text)))
   (loop for x from 0 to size do
         (setq document-vector (make-hash-table)) ;only entries for existing terms, if no entry than its zero -> less memory necessary
         (setq current-element (car free-text))
         (setq free-text (cdr free-text))
         (setq docID (first current-element))
         (setq text-content (second current-element))
         (mapcar (lambda (x)
                   (setq words (process-string x))
                   (mapcar (lambda (w) 
                             (setq index (gethash w *terms*))
                             (if (gethash index document-vector) (setf (gethash index document-vector) (+ 1 (gethash index document-vector))) ;increase term frequency
                               (setf (gethash index document-vector) 1))) ;create new entry
                           words))
                   text-content)
         (setf (gethash docID *documents*) (append (list document-vector) (gethash docID *documents*))))))
         
         


         
         
  