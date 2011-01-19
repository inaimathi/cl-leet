;; :name (capitalize (grammar->string planet-name-grammar))
;; :description (grammar->string planet-desc-grammar)
;; :radius (+ 1000 (random 7000))
;; :x (random 300) :y (random 300) :z (random 300)
;; :market (generate-market tech)
;; :government gov :economy econ :tech-level tech :population pop :productivity prod

(defun planets->stats (list-of-planets &optional acc)
  (cond ((not list-of-planets) acc)
	(t (planets->stats (cdr list-of-planets) 
			   (add-planet (car list-of-planets) acc)))))

(defun add-planet (planet stats)
  (let ((gov (add-stat (planet-government planet) (cdr (assoc 'government stats))))
	(econ (add-stat (planet-economy planet) (cdr (assoc 'economy stats))))
	(tech (add-stat (planet-tech-level planet) (cdr (assoc 'tech-level stats))))
	(pop (add-stat (planet-population planet) (cdr (assoc 'tech-level stats)))))
    `((government . ,gov)
      (economy . ,econ)
      (tech-level . ,tech)
      (population . ,pop))))

(defun add-stat (new-stat stat-list)
  (let ((stat (assoc new-stat stat-list)))
    (if stat
	(progn (incf (cdr stat))
	       stat-list)
      (cons `(,new-stat . 1) stat-list))))

(provide 'leet-stats)