;;;; Hilfsfunktionen


;;;;Mergesort
(defun mergesort (a)
  (setq m (length a))
  (if (equal m 1) (return a)
   (progn 
     (let* ((half (floor (/ m 2)))
           (aLeft (mergesort (subseq  a 0 half)))
           (aRight (mergesort (subseq a half m))))
       (merge-lists aLeft aRight)))))


(defun merge-lists (aLeft aRight)
  (let* ((size (+ (length aLeft) (length aRight)))
         (b (make-array size)))
    (loop for x from 0 to size do
          (let ((x (car aLeft))(y (car aRight))) ;get first elem
            (when (and (not (equal null x)) (not (equal null y)))
              (if (<= x y) 
                  (progn (setq aLeft (cdr aLeft)) ;set list to rest
                    (vector-push x b)) ;add smaller list item ro sorted list
                (progn (setq aRight (cdr aRight))
                  (vector-push y b)))))) ;add smaller list item ro sorted list
(mapcar (lambda (x) (vector-push x b)) (append aLeft aRight)) ;one of the lists has to be empty (x or y nil) --> this is the rest
(coerce b 'list)))


(defun intersect (p1 p2)
  (let* ((answer '()))
    (loop while (and (not (equal p1 nil)) (not (equal p2 nil))) do
          (cond ((equal (car p1) (car p2))
                 (setq answer (append answer (list (car p1))))
                 (setq p1 (cdr p1))
                 (setq p2 (cdr p2)))
                ((< (car p1) (car p2)) (setq p1 (cdr p1)))
                (t (setq p2 (cdr p2))))) answer))




     
    
;;;;--------------------------------------------------------------


(mergesort '(7 834 234 124 46 203i4 -1))

(setq a '(1 34 345 900))
(setq b '(34 50 900 12321))

(intersect a b)
a
b