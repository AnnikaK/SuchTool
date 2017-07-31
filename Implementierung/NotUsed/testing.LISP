;;;TESTING STUFF
;;;;---------------------------------------------------------------------------------------------------------

(defun test(l)
  (loop for x from 0 to (- (length l) 1) do
       (print (car l))
       (setq l (cdr l))))

(defun printDict (dict)
  (loop for key being the hash-key of dict do   
 (print key)
        (print (gethash key dict))))      


;test (cdr (split (readFileInLines "C:\\Users\\kreme\\OneDrive\\PractisingLisp\\Lisp\\mail-beispiele-eingang\\mail-beispiele-eingang\\mail-588.ldd"))))

;process-keyword-list (car (split (readFileInLines "H:\\SuchTool-develop\\mail-beispiele-eingang\\mail-beispiele-eingang\\mail-588.ldd"))) 1)

;(printDict *terms*)

;(printDict *documents*)

;(printDict *keyword-dict*)  
       

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
        (print (term-idf value))))

;(printTerms)

;(printDict *documents*)
;(printDict *query-vector*)


;(printDict (document-document-vector (gethash 2 *documents*)))




;(gethash "DFKI" *terms*)
;(create-query-vector "grillen im Mai") 
       


    

;(defvar *special-characters* '(#\# #\' #\" #\/ #\\ #\; #\- #\( #\) #\[ #\]))
;(setq term (ignore-errors (read-from-string (delete-if (lambda (c) (member c *special-characters*)) x))))







;(connect-or-total '(((0.2 15) (0.3 2)) ((0.2 4) (0.3 2))))



;(merge-and '((1 23) (1 234) (1 5)) '((0.2 23) (0.3 234) (0.23 13234)))
;(merge-or '((1 23) (1 234) (1 5)) '((0.2 23) (0.3 234) (0.23 13234)))



;;;;------------------------------------------------------------------------------------------------

; (dir "C:\\testdatei.txt")

; (datei-oeffnen "C:\\testdatei.txt")

       

         
        
       
        
       
