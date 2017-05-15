(defun displaySelection (radioButtonPanel)
  (setq tmp (capi:choice-selected-item radioButtonPanel))
  (if (equal tmp buttonAnd) 
      (print "AND")
    (print "OR")))



(defun on-add-clicked (checkButtonPanel)
  (let ((selection-list))
    (setq selection-list (capi:choice-selected-items checkButtonPanel))
    (print selection-list)))