(defvar *terms* (make-hash-table)) ;key is term, value is tuple (vector-index document-frequency)
(defvar *vocabulary* '())
(defvar *query-vector*)
(defvar *query-vector-length*)
(defvar *score-list*)
(defvar *german-stopwords* '(ab aber aehnlich ähnlich alle allein allem allen aller alles allg allgemein als also am an and ander andere anderem anderen anderer anderes anderm andern anderr anders auch auf aus außer author autor been bei beim bereits besonders besser bevor bietet bin bis bist böden boeden bzw ca da dabei dadurch dafuer daher damit daneben dann daran darauf daraus darum das dass daß dasselbe davon davor dazu dein deine deinem deinen deiner deines dem demselben den denen denn dennoch denselben der derem deren derer derselbe derselben des deshalb desselben dessen dich die dies diese dieselbe dieselben diesem diesen dieser dieses dinge dir doch dort du dunklen durch eben eher eigenen eigenes eigentlich ein eine einem einen einer eines einfach einig einige einigem einigen einiger einiges einmal er erst erste erster es etc etwa etwas euch euer eure eurem euren eurer eures fall finden for für ganz ganze ganzem ganzen ganzer ganzes gar geben gegen geht gewesen ggf gibt gleich gute guten hab habe haben hat hatte hatten hattest hattet hier hin hinter hinterher ich ihm ihn ihnen ihr ihre ihrem ihren ihrer ihres im immer in indem information ins ist ja je jede jedem jeden jeder jedes jedoch jene jenem jenen jener jenes jetzt kann kannst kein keine keinem keinen keiner keines koennen koennt kommen kommt können könnt konnte könnte langsam lassen leicht leider lesen lichten liest machen mag man manche manchem manchen mancher manches mehr mehrere mein meine meinem meinen meiner meines meist mich mir mit möchte moechte moeglich möglich muß müssen mußt müßt musste müsste nach nachdem nachher natürlich ncht neben nein neu neue neuem neuen neuer neues nicht nichts noch nun nur nutzung ob oder off ohne online per schnell schon schwierig sehen sehr sehrwohl seid sein seine seinem seinen seiner seines seit seite seiten selber selbst sich sie sieht sind so sodaß solch solche solchem solchen solcher solches soll sollen sollst sollt sollte sollten solltest sondern sonst soviel soweit sowie sowohl spielen statt steht suchen titel über um und uns unse unsem unsen unser unsere unseren unseres unter version viel viele vieles vom von vor vorher wachen während wann war waren warst warum was weg weil weiter weitere welche welchem welchen welcher welches wenig wenige weniger wenn wer werde werden werdet weshalb wie wieder wieso wieviel will wir wird wirklich wirst wo woher wohin wohl wollen wollte wurde würde wurden würden zu zum zur zwar zwischen))


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


                     (list "" word "is a stopword, will be ignored in search!" ""))
                    (make-instance
                     'capi:push-button


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





