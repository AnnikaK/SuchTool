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