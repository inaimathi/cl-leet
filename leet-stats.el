;; :name (capitalize (grammar->string planet-name-grammar))
;; :description (grammar->string planet-desc-grammar)
;; :radius (+ 1000 (random 7000))
;; :x (random 300) :y (random 300) :z (random 300)
;; :market (generate-market tech)
;; :government gov :economy econ :tech-level tech :population pop :productivity prod

(defvar *test-gal* nil)
(defvar *stats* nil)

(defun generate-name-data ()
  (interactive)
  (with-current-buffer (get-buffer-create "*graph*")
    (let ((names (mapcar 'grammar->string (make-list 3000 planet-name-grammar))))
      (print-graph (cons "planet-names" (list->data names))))
    (display-buffer "*graph*")))

(defun generate-data-set ()
  (interactive)
  (progn (reset-data)
	 (graph-galaxy *test-gal*)))

(defun reset-data ()
  (progn 
    (setf *test-gal* (mapcar (lambda (n) (generate-planet)) (make-list 3000 0)))
    (setf *stats* `((government . nil)
		    (economy . nil)
		    (tech-level . nil)
		    (population . nil)
		    (market-mod . nil)
		    (radius . nil)))))

(defun graph-galaxy (gal)
  (progn (planets->stats! gal)
	 (display-stats *stats*)))

(defun planets->stats! (list-of-planets)
  (mapc 'add-planet! list-of-planets))

(defun add-planet! (p)
  (progn
    (add-stat! 'government (planet-government p))
    (add-stat! 'economy (planet-economy p))
    (add-stat! 'tech-level (planet-tech-level p))
    (add-stat! 'population (planet-population p))
    (add-stat! 'market-mod (max 10 (+ (- (* (planet-economy p) (planet-productivity p)) (planet-government p)) (planet-tech-level p))))
    (add-stat! 'radius (planet-radius p))))

(defun add-stat! (stat addition)
  (let ((existing (cdr (assoc addition (cdr (assoc stat *stats*))))))
    (if existing
	(incf (cdr (assoc addition (cdr (assoc stat *stats*)))))
      (setf (cdr (assoc stat *stats*)) (cons `(,addition . 1) (cdr (assoc stat *stats*)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graph Printing (game-independant) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun list->data (a-list)
  (let ((acc nil))
    (progn (mapc (lambda (elem)
		   (let ((existing (assoc elem acc)))
		     (cond (existing (incf (cdr existing)))
			   (t (setf acc (cons `(,elem . 1) acc))))))
		 a-list)
	   acc)))

(defun display-stats (stat-alist)
  (progn (with-current-buffer (get-buffer-create "*graph*")
	   (mapc 'print-graph stat-alist))
	 (display-buffer "*graph*")))

(defun print-graph (graph)
  (let ((title (car graph))
	(data (mapcar (lambda (p) (cons (car p) (make-string (cdr p) 124)))
		      (sort (cdr graph) (lambda (a b) 
					  (if (stringp (car a))
					      (string< (car a) (car b))
					    (< (car a) (car b))))))))
    (progn (insert (format "\n\n%20s\n" title))
	   (mapc (lambda (a) (insert (format "%10s: %s\n" (car a) (cdr a)))) data))))

(provide 'leet-stats)
