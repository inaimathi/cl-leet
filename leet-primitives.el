(defun filter (predicate lst &optional acc) ;; TODO:Generalize this for sequences when you get around to it (Elisp convention seems to be to take a sequence and return a list in this situation)
  (cond ((not lst) (reverse acc))
	((funcall predicate (car lst)) (filter predicate (cdr lst) (cons (car lst) acc)))
	(t (filter predicate (cdr lst) acc))))

(defun pick (a-list) (nth (random (length a-list)) a-list))
(defun pick-g (key grammar) (pick (gethash key grammar))) ;;pick specialized to grammars

(defun roll-dice (num-dice die-type &optional mod) ;;The simplest non-uniform dice roller I could think up without resorting to the grab-bag
  (let* ((rolls (mapcar (lambda (die) (+ 1 (random die)))
			(make-list num-dice die-type))))
    (apply '+ (cons (or mod 0) rolls))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; These were used to help generate the planet-name grammar from a bunch of cool-sounding names
(defun break-string (str fragment-length)
  (let ((frag-str (number-sequence 0 (/ (length str) fragment-length))))
    (remove "" (mapcar (lambda (i)
			 (substring str (* i fragment-length) 
				    (min (length str)
					 (+ fragment-length (* i fragment-length)))))
		       frag-str))))

(defun partition-string (str)
  (let ((broken (break-string str 2)))
    (list (car broken)
	  (butlast (cdr broken))
	  (car (last broken)))))

(defun word-list->planet-grammar (list-of-words)
  "Expects a bunch of words separated by newlines or spaces. Elite for Emacs 0.1 had a bunch of planet names generated; this was the easiest way of getting a grammar that included all of them"
  (let ((words (split-string list-of-words)) ;;(with-current-buffer (buffer-string))
	(starter) (link) (ender))
    (mapcar (lambda (a-word)
	      (let ((p (partition-string a-word)))
		(add-to-list 'starter (car p))
		(mapcar (lambda (a) (add-to-list 'link a)) (cadr p))
		(add-to-list 'ender (caddr p))))
	    words)
    (list starter link ender)))

(provide 'leet-primitives)