
;;;;Initial Setup --------------------------------------------------------------------------------------------------------------------

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



;form: dictionary of keywords with keyword-name [symbol] as key and entries in form of (docID type content)
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



(defun sort-keyword-list()
  (let ((entry-list))
    (loop for key being the hash-key of *keyword-dict* do
          (setq entry-list (gethash key *keyword-dict*))
          (stable-sort entry-list #'(lambda (x y)
                       (< (keyword-entry-doc-id x) (keyword-entry-doc-id y)))))))




;has to do all init steps, called when directory selected
(defun onSelected (directory-name)
  (let ((current-doc))
    (setf (capi:button-enabled select-button) nil)
    (setq *results* '()) 
    (createDocumentDictionary directory-name)
    (loop for docID being the hash-key of *documents* do 
          (setq current-doc (split (readFileInLines (document-path (gethash docID *documents*)))))
          (process-keyword-list (first current-doc) docID)
          (setq *free-text* (cons (list docID (second current-doc)) *free-text*))
          (add-to-vocabulary (second current-doc)))
    (sort-keyword-list)
    (create-term-dictionary)
    (create-document-vectors *free-text*)
    (getKeys *keyword-dict*)))

;;;;Keyword-Search-------------------------------------------------------------------------------------------------

;logical procession of query
(defun verarbeite-query (selection-list text)
  (let ((entry-list) (entry) (type) (content) (result-list '()) (keyword-result) (tmp))
    (cond  ((> (length selection-list) 0) ;only react if user selected something
            (mapcar (lambda (keyword) ;iterate over keywords
                      (print keyword)
                      (setq entry-list (gethash keyword *keyword-dict*))
                      (setq keyword-result '()) ;contains all documents in which the current keyword contains specified text
                      (mapcar (lambda (entry)  ;iterate over all entries for current keyword
                                (setq docID (keyword-entry-doc-id entry))
                                (setq type (keyword-entry-keyword-type entry))
                                (setq content (keyword-entry-content entry))
                                (cond ((and (equal type 'cons) (recursive-list-search text content)) ;different search procedures according to type
                                       (setq keyword-result (cons docID keyword-result)))
                                      ((and (equal type 'simple-base-string) (string-search text content)) ;union is necessary if keyword occurs multiple times in document (e.g. different cases)
                                       (setq keyword-result (union (list docID) keyword-result))) 
                                      ((and (equal type 'fixnum) (num-search text content))
                                       (setq keyword-result (union (list docID) keyword-result)))))
                              entry-list)
                      (setq result-list (cons keyword-result result-list)))
                    selection-list)

            
                   (if (equal (capi:choice-selected-item radio-buttons) button-or) 
                       (setq result-list (connect-or result-list)) ;connect inner term with and/or
                     (setq result-list (setq result-list (connect-and result-list))))


            (cond ((not (null (member 'NOT (capi:choice-selected-items not-operator-panel))))  ;if selected, surround with not
                   (setq result-list (negate result-list)))))) result-list)) 


(defun recursive-list-search (text content)
  (let ((elem) (found nil))
    (cond ((null (car content)) nil)
           (t (setq elem (car content)) ;erstes element durchsuchen
              (cond ((equal (type-of elem) 'simple-base-string) (setq found (string-search text elem)))
                    ((equal (type-of elem) 'fixnum) (setq found (num-search text elem)))
                    ((equal (type-of elem) 'cons) (setq found (recursive-list-search text elem)))) ;sublisten auch durchsuchen
              (if found t (recursive-list-search text (cdr content)))))))
          
              


(defun string-search (text content)
  (search text content :test 'equalp))




(defun num-search (text content)
  (let ((num (ignore-errors (read-from-string text))))
    (cond ((numberp num) (= num content))
          (t (cond ((equalp text "null") (setq num 0))
                   ((equalp text "eins") (setq num 1))
                   ((equalp text "zwei") (setq num 2))
                   ((equalp text "drei") (setq num 3))
                   ((equalp text "vier") (setq num 4))
                   ((equalp text "f�nf") (setq num 5))
                   ((equalp text "sechs") (setq num 6))
                   ((equalp text "sieben") (setq num 7))
                   ((equalp text "acht") (setq num 8))
                   ((equalp text "neun") (setq num 9))
                   ((equalp text "zehn") (setq num 10))
                   ((equalp text "elf") (setq num 11))
                   ((equalp text "zw�lf") (setq num 12)))
             (if (numberp num) (= num content))))))




(defun connect-and (result-list)
 (let ((and-res))
  (setq result-list (sort result-list '> :key 'length))
  (setq and-res (car result-list))
  (setq result-list (cdr result-list))
  (mapcar (lambda (x) 
            (setq and-res (intersection and-res x))) result-list)
  and-res))
    


(defun connect-or (result-list)
  (let ((or-res))
    (setq or-res (car result-list))
    (setq result-list (cdr result-list))
    (mapcar (lambda (x)
              (setq or-res (union or-res x))) result-list)
    or-res))
    

(defun negate (result)
  (let ((all-docs '()))
    (loop for id being the hash-key of *documents* do
          (setq all-docs (cons id all-docs)))
    (setq all-docs (set-difference all-docs result))
    all-docs))


;;;; Package Split-Sequence, taken from https://github.com/sharplispers/split-sequence    
;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; SPLIT-SEQUENCE
;;;
;;; This code was based on Arthur Lemmens' in
;;; <URL:http://groups.google.com/groups?as_umsgid=39F36F1A.B8F19D20%40simplex.nl>;
;;;
;;; changes include:
;;;
;;; * altering the behaviour of the :from-end keyword argument to
;;; return the subsequences in original order, for consistency with
;;; CL:REMOVE, CL:SUBSTITUTE et al. (:from-end being non-NIL only
;;; affects the answer if :count is less than the number of
;;; subsequences, by analogy with the above-referenced functions).
;;;
;;; * changing the :maximum keyword argument to :count, by analogy
;;; with CL:REMOVE, CL:SUBSTITUTE, and so on.
;;;
;;; * naming the function SPLIT-SEQUENCE rather than PARTITION rather
;;; than SPLIT.
;;;
;;; * adding SPLIT-SEQUENCE-IF and SPLIT-SEQUENCE-IF-NOT.
;;;
;;; * The second return value is now an index rather than a copy of a
;;; portion of the sequence; this index is the `right' one to feed to
;;; CL:SUBSEQ for continued processing.

;;; There's a certain amount of code duplication here, which is kept
;;; to illustrate the relationship between the SPLIT-SEQUENCE
;;; functions and the CL:POSITION functions.

(defpackage :split-sequence
  (:use :common-lisp)
  (:export #:split-sequence
           #:split-sequence-if
           #:split-sequence-if-not))

(in-package :split-sequence)

(deftype array-index (&optional (length array-dimension-limit))
  `(integer 0 (,length)))

(declaim (ftype (function (&rest t) (values list integer))
                split-sequence split-sequence-if split-sequence-if-not))

(declaim (ftype (function (function sequence array-index
                                    (or null array-index) (or null array-index) boolean)
                          (values list integer))
                split-from-start split-from-end))

(macrolet ((check-bounds (sequence start end)
             (let ((length (gensym (string '#:length))))
               `(let ((,length (length ,sequence)))
                  (check-type ,start unsigned-byte "a non-negative integer")
                  (when ,end (check-type ,end unsigned-byte "a non-negative integer or NIL"))
                  (unless ,end
                    (setf ,end ,length))
                  (unless (<= ,start ,end ,length)
                    (error "Wrong sequence bounds. start: ~S end: ~S" ,start ,end))))))

  (defun split-sequence (delimiter sequence &key (start 0) (end nil) (from-end nil)
                         (count nil) (remove-empty-subseqs nil)
                         (test #'eql) (test-not nil) (key #'identity))
    "Return a list of subsequences in seq delimited by delimiter.
If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE.  In particular, the
behaviour of :from-end is possibly different from other versions of
this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
    (check-bounds sequence start end)
    (cond
      ((and (not from-end) (null test-not))
       (split-from-start (lambda (sequence start)
                           (position delimiter sequence :start start :key key :test test))
                         sequence start end count remove-empty-subseqs))
      ((and (not from-end) test-not)
       (split-from-start (lambda (sequence start)
                           (position delimiter sequence :start start :key key :test-not test-not))
                         sequence start end count remove-empty-subseqs))
      ((and from-end (null test-not))
       (split-from-end (lambda (sequence end)
                         (position delimiter sequence :end end :from-end t :key key :test test))
                       sequence start end count remove-empty-subseqs))
      (t
       (split-from-end (lambda (sequence end)
                         (position delimiter sequence :end end :from-end t :key key :test-not test-not))
                       sequence start end count remove-empty-subseqs))))

  (defun split-sequence-if (predicate sequence &key (start 0) (end nil) (from-end nil)
                            (count nil) (remove-empty-subseqs nil) (key #'identity))
    "Return a list of subsequences in seq delimited by items satisfying
predicate.
If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE-IF.  In particular, the
behaviour of :from-end is possibly different from other versions of
this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
    (check-bounds sequence start end)
    (if from-end
        (split-from-end (lambda (sequence end)
                          (position-if predicate sequence :end end :from-end t :key key))
                        sequence start end count remove-empty-subseqs)
        (split-from-start (lambda (sequence start)
                            (position-if predicate sequence :start start :key key))
                          sequence start end count remove-empty-subseqs)))

  (defun split-sequence-if-not (predicate sequence &key (count nil) (remove-empty-subseqs nil)
                                (from-end nil) (start 0) (end nil) (key #'identity))
    "Return a list of subsequences in seq delimited by items satisfying
\(CL:COMPLEMENT predicate).
If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE-IF-NOT.  In particular,
the behaviour of :from-end is possibly different from other versions
of this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
    (check-bounds sequence start end)
    (if from-end
        (split-from-end (lambda (sequence end)
                          (position-if-not predicate sequence :end end :from-end t :key key))
                        sequence start end count remove-empty-subseqs)
        (split-from-start (lambda (sequence start)
                            (position-if-not predicate sequence :start start :key key))
                          sequence start end count remove-empty-subseqs))))

(defun split-from-end (position-fn sequence start end count remove-empty-subseqs)
  (declare (optimize (speed 3) (debug 0)))
  (loop
     :for right := end :then left
     :for left := (max (or (funcall position-fn sequence right) -1)
                       (1- start))
     :unless (and (= right (1+ left))
                  remove-empty-subseqs) ; empty subseq we don't want
     :if (and count (>= nr-elts count))
     ;; We can't take any more. Return now.
       :return (values (nreverse subseqs) right)
     :else
       :collect (subseq sequence (1+ left) right) into subseqs
       :and :sum 1 :into nr-elts
     :until (< left start)
   :finally (return (values (nreverse subseqs) (1+ left)))))

(defun split-from-start (position-fn sequence start end count remove-empty-subseqs)
  (declare (optimize (speed 3) (debug 0)))
  (let ((length (length sequence)))
    (loop
       :for left := start :then (+ right 1)
       :for right := (min (or (funcall position-fn sequence left) length)
                          end)
       :unless (and (= right left)
                    remove-empty-subseqs) ; empty subseq we don't want
       :if (and count (>= nr-elts count))
       ;; We can't take any more. Return now.
         :return (values subseqs left)
       :else
         :collect (subseq sequence left right) :into subseqs
         :and :sum 1 :into nr-elts
       :until (>= right end)
     :finally (return (values subseqs right)))))

(pushnew :split-sequence *features*)

;;;;Vector Space Model----------------------------------------------------------------------------------------
(defvar *terms* (make-hash-table)) ;key is term, value is tuple (vector-index document-frequency)
(defvar *vocabulary* '())
(defvar *query-vector*)
(defvar *query-vector-length*)
(defvar *score-list*)
(defvar *german-stopwords* '(ab aber aehnlich ähnlich alle allein allem allen aller alles allg allgemein als also am an and ander andere anderem anderen anderer anderes anderm andern anderr anders auch auf aus außer author autor been bei beim bereits besonders besser bevor bietet bin bis bist böden boeden bzw ca da dabei dadurch dafuer daher damit daneben dann daran darauf daraus darum das dass daß dasselbe davon davor dazu dein deine deinem deinen deiner deines dem demselben den denen denn dennoch denselben der derem deren derer derselbe derselben des deshalb desselben dessen dich die dies diese dieselbe dieselben diesem diesen dieser dieses dinge dir doch dort du dunklen durch eben eher eigenen eigenes eigentlich ein eine einem einen einer eines einfach einig einige einigem einigen einiger einiges einmal er erst erste erster es etc etwa etwas euch euer eure eurem euren eurer eures fall finden for f�r ganz ganze ganzem ganzen ganzer ganzes gar geben gegen geht gewesen ggf gibt gleich gute guten hab habe haben hat hatte hatten hattest hattet hier hin hinter hinterher ich ihm ihn ihnen ihr ihre ihrem ihren ihrer ihres im immer in indem information ins ist ja je jede jedem jeden jeder jedes jedoch jene jenem jenen jener jenes jetzt kann kannst kein keine keinem keinen keiner keines koennen koennt kommen kommt können könnt konnte könnte langsam lassen leicht leider lesen lichten liest machen mag man manche manchem manchen mancher manches mehr mehrere mein meine meinem meinen meiner meines meist mich mir mit möchte moechte moeglich möglich muß müssen mußt müßt musste müsste nach nachdem nachher nat�rlich ncht neben nein neu neue neuem neuen neuer neues nicht nichts noch nun nur nutzung ob oder off ohne online per schnell schon schwierig sehen sehr sehrwohl seid sein seine seinem seinen seiner seines seit seite seiten selber selbst sich sie sieht sind so sodass solch solche solchem solchen solcher solches soll sollen sollst sollt sollte sollten solltest sondern sonst soviel soweit sowie sowohl spielen statt steht suchen titel über um und uns unse unsem unsen unser unsere unseren unseres unter version viel viele vieles vom von vor vorher wachen während wann war waren warst warum was weg weil weiter weitere welche welchem welchen welcher welches wenig wenige weniger wenn wer werde werden werdet weshalb wie wieder wieso wieviel will wir wird wirklich wirst wo woher wohin wohl wollen wollte wurde würde wurden würden zu zum zur zwar zwischen))


(defvar *english-stopwords* '(a able about across after all almost also am among an and any are as at be because been but by can cannot could dear did do does either else ever every for from get got had has have he her hers him his how however i if in into is it its just least let like likely may me might most must my neither no nor not of off often on only or other our own rather said say says she should since so some than that the their them then there these they this tis to too twas us wants was we were what when where which while who whom why will with would yet you your ain't aren't can't could've couldn't didn't doesn't don't hasn't he'd he'll he's how'd how'll how's i'd i'll i'm i've isn't it's might've mightn't must've mustn't shan't she'd she'll she's should've shouldn't that'll that's there's they'd they'll they're they've wasn't we'd we'll we're weren't what'd what's when'd when'll when's where'd where'll where's who'd who'll who's why'd why'll why's won't would've wouldn't you'd you'll you're you've))


(defvar *stopword-dict-ger* (make-hash-table :test 'equalp))
(defvar *stopword-dict-engl* (make-hash-table :test 'equalp))

(defun setup-stopword-dict-ger ()
  (mapcar (lambda (word)
            (setf (gethash word *stopword-dict-ger*) t))
          *german-stopwords*)
  (setq *german-stopwords* nil))


(defun setup-stopword-dict-engl ()
  (mapcar (lambda (word)
            (setf (gethash word *stopword-dict-engl*) t))
          *english-stopwords*)
  (setq *english-stopwords* nil))





(defun add-to-vocabulary (doc-text)
  (let ((words))
    (setq vocabulary '())
    (setup-stopword-dict-ger)
    (setup-stopword-dict-engl)
    (mapcar (lambda (x)
              (setq words (process-string x))
              (mapcar (lambda (y) 
                        (cond ((and (not (gethash (ignore-errors (read-from-string y)) *stopword-dict-ger*)) 
                                    (not (gethash (ignore-errors (read-from-string y)) *stopword-dict-engl*))
                                    (not (member y *vocabulary*)))
                               (setq *vocabulary* (cons y *vocabulary*)))))
                      words))
            doc-text)))




(defun process-string (str) 
  (split-sequence:SPLIT-SEQUENCE #\Space  str :remove-empty-subseqs t))





(defun create-term-dictionary ()
  (let ((index 0) (term))
    (setq *terms* (make-hash-table :test 'equalp))
    (mapcar (lambda (x)
                     (setq term x) 
                     (setf (gethash term *terms*) (make-term :index index :idf 0))
                     (setq index (+ 1 index)))
              *vocabulary*)))
  
 

 
(defun create-document-vectors (free-text)
 (let ((size (length free-text)) (current-element) (docID) (text-content) (doc-vector) (alreadySet) (words) (current-term) (wordindex)) 
   (loop for x from 0 to size do
         (setq current-element (car free-text)) ;(docID text-content) , text-content is list of line-strings
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
                                           (cond ((equal (gethash wortindex alreadySet) nil) ;increase doc-freuency (later transformed to idf)
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
                 (setq idf (log (/ number-of-documents df) 10)) 
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
             (cond ((or (gethash (ignore-errors (read-from-string word)) *stopword-dict-ger*) (gethash (ignore-errors (read-from-string word)) *stopword-dict-engl*))
                    (show-stopword-occured word)))
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

;;;;Schnittstelle Logik-UI--------------------------------------------------------------------------------------------------

(defun on-add-clicked (text)
  (let ((selection-list) (free-text-results '()) (keyword-results '()) (no-error nil) (total-results '()) (free-text-search-included nil))
    (setq selection-list (capi:choice-selected-items keyword-panel))
    (cond ((not (equal nil selection-list))

           (display-query selection-list text)
    ;free-text-search
           (cond ((member 'FREITEXT selection-list)
                  (setq free-text-search-included t)
                  (setq free-text-results (on-free-text-query-submitted text)) ;(score docID)
                  (setq selection-list (remove 'FREITEXT selection-list)))) 
    ;keyword-search
           (setq keyword-results (verarbeite-query selection-list text))
    ;get total result list for current query-term
           (setq keyword-results (mapcar (lambda (x) (list 1 x)) keyword-results))
           ;set total results for this query
           (cond (free-text-search-included
                  (cond 
                   ((equal (length selection-list) 0)(setq total-results free-text-results)) ;free-text search alone

                   ((equal (capi:choice-selected-item radio-buttons) button-and) ;connect with and
                    (setq total-results (merge-and keyword-results free-text-results)))

                   ((equal (capi:choice-selected-item radio-buttons) button-or) ;connect with or
                    (setq total-results (merge-or keyword-results free-text-results)))))

                 (t (setq total-results keyword-results)))  ;keyword-search alone

           
           (cond ((and (> (length *results*) 0) (equal (capi:choice-selected-item buttons-outer-term) and-outer-term)) ;connect with compplete query
                  (setq *results* (merge-and total-results *results*)))  
                 ((and (> (length *results*) 0) (equal (capi:choice-selected-item buttons-outer-term) or-outer-term))
                  (print *results*)  (setq *results* (merge-or total-results *results*)) (print *results*))
                 (t (setq *results* total-results))))

          (t (show-error)))))




(defun merge-and (a b)
  (let ((res '()) (all (list a b)) (smaller) (larger))
    (cond ((and (> (length a) 0) (> (length b) 0))
           (setq all (sort all '< :key 'length))
           (setq smaller (first all))
           (setq larger (second all))
           (mapcar (lambda (x)
                     (setq score (car (member (second x) larger :test '= :key 'second)))
                     (cond ((not (equal nil score)) 
                            (setq new-score (list (+ (car x) (car score)) (second x))) 
                            (setq res (cons new-score res))))) smaller)))
    res)) 



(defun merge-or (a b)
  (let ((res '()) (all (list a b)) (smaller) (larger))
    (setq all (sort all '< :key 'length))
    (setq smaller (first all))
    (setq larger (second all))
    (mapcar (lambda (x)
              (setq score (car (member (second x) larger :test '= :key 'second)))
              (cond 
               ((not (equal nil score)) 
                (setq new-score (list (+ (car x) (car score)) (second x))) 
                (setq res (cons new-score res))
                (setq larger (remove score larger)))
               (t (setq res (cons x res))))) smaller)
    (setq res (append larger res))
    res))




 

(defun display-query (selection-list text)
  (let ((and t) (displayed-query) (tmp "") (before "") (middle "") (after ""))
    (cond ((equal (capi:choice-selected-item radio-buttons) button-or) (setq and nil))) 
    (setq displayed-query (capi:display-pane-text query-display))
    ;verknuepfung intern
    (cond ((> (length selection-list) 1)
           (if and (setq before " (and ") (setq before " (or "))
           (setq after ") ")))
    ;surround with not
    (cond ((equal '(not) (capi:choice-selected-items not-operator-panel))
           (setq before (concatenate 'string " (not " before))
           (setq after (concatenate 'string after ") "))))
    
     ;create bracket-expression for each keyword
    (mapcar (lambda (x) 
              (setq tmp (concatenate 'string " (= " (write-to-string x) " \""  text  "\") "))
              (setq middle (concatenate 'string middle tmp))) selection-list)
    (setq tmp (concatenate 'string before middle after))
    ;verknuepfung extern
    (cond ((> (length displayed-query) 0)
           (cond ((equal (capi:choice-selected-item buttons-outer-term) and-outer-term)
                  (setq displayed-query (concatenate 'string "(AND " displayed-query)))
                 ((equal (capi:choice-selected-item buttons-outer-term) or-outer-term)
                  (setq displayed-query (concatenate 'string "(OR " displayed-query))))
           (setq tmp (concatenate 'string tmp ")"))))

    (setf (capi:display-pane-text query-display) (concatenate 'string displayed-query tmp))))


(defun show (top)
  (let ((string-list '()) (tmp) (index (length top)) (score) (current-elem '()))
    (setq top (sort top '< :key 'car))
    (mapcar (lambda (x) 
              (setq score (write-to-string (first x)))
              (setq score (subseq score 0 (min (length score) 7)))
              (setq datum (document-datum (gethash (second x) *documents*)))
              (setq absender (document-absender (gethash (second x) *documents*)))
              (if (equal nil datum) (setq datum ""))
              (if (equal nil absender) (setq absender ""))
                    

              (setq tmp (list (concatenate 'string (write-to-string index) ".      ")
                              score
                              datum
                              absender
                              (namestring (document-path (gethash (second x) *documents*)))))
              (setq string-list (cons tmp string-list))
              (setq index (- index 1))) top)
    (show-results string-list)))





(defun show-results (result-list)
  (let  ((breite 1000))
    (setq height (+ 30 (round (reduce 'min (list (* 11.5 (length result-list)) 300)))))
    (capi:display-dialog 
     (capi:make-container
 
      (make-instance 'capi:column-layout
                     :x-adjust  :left
                     :min-width breite
                     :background :white
                     :gap 20
                     :description
                     
                       
                     (list
                      ""

                                      
                      (setq open-mode-panel (make-instance 
                                             'capi:radio-button-panel
                       
                                             :items (list 
                                                     (setq dir-button (make-instance 'capi:radio-button
                                                                                     :text "Open directory"
                                                                                     :selected t))
                                                     (setq file-button (make-instance 'capi:radio-button
                                                                                      :text "Open file"
                                                                                      :selected nil)))))

                                 
                      (make-instance 'capi:row-layout 
                                     :gap 20
                                     :description (list ""

                                                        (setq result-table (make-instance 'capi:multi-column-list-panel


                                     
                                                                                          :visible-min-width 600
                                                                                          :visible-min-height height
                                                                                          :vertical-scroll t
                                                                                          :horizontal-scroll t
                                                                                          :action-callback 'result-selected
                                                                                          :columns '((:title "Rank"
                                                                                                      :adjust :left
                                                                                                      :visible-min-width (character 10))
                                                                                                     (:title "Score"
                                                                                                      :adjust :left
                                                                                                      :visible-min-width (character 15))
                                                                                                     (:title "Datum"
                                                                                                      :adjust :left 
                                                                                                      :visible-min-width (character 30))
                                                                                                     (:title "Absender"
                                                                                                      :adjust :left 
                                                                                                      :visible-min-width (character 60))                                                                                 
                                                                                                     (:title "Dateipfad"
                                                                                                      :adjust :left 
                                                                                                      :visible-min-width (character 70)))

                                                                                          :items result-list))"" ))
      
                      "" 
                     

                      (make-instance 'capi:row-layout
                                     :x-adjust  :center
                                     :min-width breite
                                     :description (list (make-instance
                                                         'capi:push-button
                                                         :text "OK"  
                                                         :callback-type :data
                                                         :callback 
                                                         #'(lambda (arg)  
                                                             (capi:exit-dialog arg)))))))))))





(defun show-error ()
  (capi:display-dialog  
   (capi:make-container
    (make-instance 'capi:column-layout
                   :x-adjust  :center
                   :width 200
                   :description
                   (list 
                     
                    (make-instance 
                     'capi:row-layout :description 
                     (list "Please select at least one keyword!" ""))
                    (make-instance
                     'capi:push-button
                     :text "OK"  
                     :callback-type :data
                     :callback 
                     #'(lambda (arg)  
                         (capi:exit-dialog arg))))
                   :min-width 200
                   :background  :white))))


(defun show-stopword-occured (word)
  (capi:display-dialog  
   (capi:make-container
    (make-instance 'capi:column-layout
                   :x-adjust  :center
                   :width 220
                   :description
                   (list 
                     
                    (make-instance 
                     'capi:row-layout :description 
                     (list "" word "is a stopword, will be ignored in search!" ""))
                    (make-instance
                     'capi:push-button
                     :text "OK"  
                     :callback-type :data
                     :callback 
                     #'(lambda (arg)  
                         (capi:exit-dialog arg))))
                   :min-width 220
                   :background  :white))))


(defun result-selected (&rest args)
  (let* ((selected (car args)) (path (fifth selected))) 
    (cond ((equal (capi:choice-selected-item open-mode-panel) dir-button)
           (dir path))
          (t (datei-oeffnen path)))))

(defun dir (arg) 
  (sys:call-system (concatenate 'string "explorer /select,"  "\"" arg "\"")))


(defun datei-oeffnen  (arg)
  (sys:call-system (concatenate 'string "\"" arg "\"")))

  
;;;;User Interface ------------------------------------------------------------------------------------------------
(capi:define-interface ui () 
  ()    
  (:layouts
   (default-layout
    capi:column-layout
    (list 
     "Select directory:" 
     (make-instance
      'capi:row-layout 
      :gap 20
      :description
      (list 
       (setq path-text 
             (make-instance 'capi:display-pane
                            :text "C:\\"
                            :min-width 900
                            :max-width 900))
       (setq select-button (make-instance 'capi:button 
                      :text "select" 
                      :callback #'(lambda (&rest args)
                                    (let (antwort)
                                      (setq antwort (capi:prompt-for-directory "Select directory" )) 
                                      (cond (antwort 
                                                                                                  
                                             (setq antwort (namestring antwort))
                                             (setf (capi:display-pane-text path-text) antwort)
                                             (setq itemList (onSelected antwort))

                                             (setf (capi:collection-items keyword-panel) itemList)))))))))
     "Query:"

     (make-instance 
      'capi:row-layout
      :gap 20
      :description
      (list (setq query-display
                  (make-instance  'capi:display-pane   
                                  :text "" 
                                  :min-height 100
                                  :max-height 100
                                  :min-width 900 
                                  :max-width 900))
            (make-instance  'capi:button
                            :text "search" 
                            :callback #'(lambda (&rest args)
                                          (cond ((> (length (capi:display-pane-text query-display)) 0)
                                                 (show *results*)))))  
            (make-instance  'capi:button
                            :text "reset" 
                            :callback #'(lambda (&rest args) 
                                          (setf (capi:display-pane-text query-display) "") (setq *results* '())))))


     (make-instance 
      'capi:row-layout
      :gap 20
      :description
      (list 
       "Connect multiple query-terms with: "
       (setq buttons-outer-term 
             (make-instance 
              'capi:radio-button-panel
              :items (list 
                      (setq and-outer-term 
                            (make-instance 'capi:radio-button
                                           :text "AND"
                                           :selected t
                                           :selection-callback 
                                           #'(lambda  (a interface &rest args) 
                                               (setq radio-button 1))))
                      (setq or-outer-term 
                            (make-instance 'capi:radio-button
                                           :text "OR"
                                           :selected nil
                                           :selection-callback 
                                           #'(lambda  (a interface &rest args) 
                                               (setq radio-button 2)))))))))
     "" ""

     (make-instance 'capi:row-layout
                    :gap 20
                    :description 
                    (list 
                     (setq search-field 
                           (make-instance 'capi:text-input-pane 
                                          :text "search for..."
                                          :min-width 900
                                          :max-width 900
                                          :min-height 100
                                          :max-height 100
                                          :change-callback #'(lambda (text &rest args)          
                                                               (setq *text* text))))


                     (make-instance 'capi:column-layout 
                                    :gap 20 
                                    :description 
                                    (list 
                                     (make-instance 'capi:row-layout 
                                                    :gap 20 
                                                    :description 
                                                    (list 
                                                     (make-instance 
                                                      'capi:button 
                                                      :text "reset" 
                                                      :callback #'(lambda (&rest args) 
                                                                    (setf (capi:text-input-pane-text search-field) "")))))
                 
                 
                                     (make-instance 'capi:row-layout 
                                                    :gap 20
                                                    :description 
                                                    (list 
                                                     (setq radio-buttons 
                                                           (make-instance 'capi:radio-button-panel
                                                                          :items (list 
                                                                                  (setq button-and 
                                                                                        (make-instance 'capi:radio-button
                                                                                                       :text "AND"
                                                                                                       :selected t
                                                                                                       :selection-callback 
                                                                                                       #'(lambda  (a interface &rest args) 
                                                                                                           (setq radio-button 1))))
                                                                                  (setq button-or 
                                                                                        (make-instance 'capi:radio-button
                                                                                                       :text "OR"
                                                                                                       :selected nil
                                                                                                       :selection-callback 
                                                                                                       #'(lambda  (a interface &rest args) 
                                                                                                           (setq radio-button 2)))))))
                            
                                                     (make-instance 
                                                      'capi:button 
                                                      :text "add" 
                                                      :callback #'(lambda (&rest args) 
                                                                    (on-add-clicked (capi:text-input-pane-text search-field))))))

                                     (setq not-operator-panel 
                                           (make-instance 'capi:check-button-panel
                                                          :min-width 900
                                                          :layout-class 'capi:column-layout
                                                          :items   '(NOT))))))) 
     "" "Keys:"

     (make-instance 'capi:column-layout 
                    :vertical-scroll t
                    :description (list
                                  (setq keyword-panel
                                        (make-instance 'capi:check-button-panel
                                                       :min-width 900
                                                       :layout-class 'capi:column-layout
                                                       :items   nil)))))
    :gap 20
    :x 0
    :min-width 1124
    :min-height 800
    :y 0
    :x-adjust :left
    :internal-border 20)))






(defun open-ui(&rest args) 
  (capi:display 
   (make-instance 'capi:interface
    :title "Such-Tool"
    :x 20
    :y 100
    :layout (list
             (make-instance 'capi:row-layout 
                            :description
                            (list 
                             (make-instance 'ui)))))))



(open-ui)







   
                               


