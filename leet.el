(require 'leet-data)

(defun filter (predicate lst &optional acc)
  (cond ((not lst) (reverse acc))
	((funcall predicate (car lst)) (filter predicate (cdr lst) (cons (car lst) acc)))
	(t (filter predicate (cdr lst) acc))))

(defun generate-planet ()
  (let* ((gov (random 8))
	 (econ (if (> gov 0) (logior (logand (lsh (random 3) -8) 7) 2) (logand (lsh (random 3) -8) 7)))
	 (tech (+ (logand (lsh (random 3) -8) 3) (logxor econ 7) (lsh gov -1)))
	 (pop (+ (* 4 tech) econ gov 1))
	 (prod (* (+ (logxor econ 7) 3) (+ 4 gov) pop 8)))
    (make-planet :name (capitalize (grammar->string planet-name-grammar))
		 :description (grammar->string planet-desc-grammar)
		 :radius (+ 1000 (random 7000))
		 :x (random 300) :y (random 300) :z (random 300)
		 :market '((widgets 30 10) (gewgaws 30 10) (whasits 50 10))
		 :government gov :economy econ :tech-level tech :population pop :productivity prod
		 :stats (list :gov gov :econ econ :tech tech :pop pop :prod prod)))) ;; numeric versions of these stats, in case I need to recalculate something later

(defvar galaxy (mapcar (lambda (n) (generate-planet)) (make-list 15 0)))
(defvar commander (make-captain :name "Mal"
				:credits 10000
				:reputation 0
				:xp 0
				:current-planet (planet-name (car galaxy))
				:trade-history '()
				:ship (make-ship :name "Serenity"
						 :cargo-cap 10
						 :cargo nil
						 :frame 'firefly
						 :engine 'standard
						 :speed 20
						 :fuel-consumption 1
						 :fuel-cap 150
						 :fuel 150)))

;; Commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cap-info () 
  (interactive)
  (insert (captain-info commander)))

(defun plan-info ()
  (interactive)
  (insert (planet-info (planet-name->planet (captain-current-planet commander)))))

(defun market ()
  (interactive)
  (mapcar 'insert 
	  (market-info (planet-market (planet-name->planet (captain-current-planet commander))))))

(defun local-planets ()
  (interactive)
  (mapcar (lambda (p) (insert p "\n"))
	  (list-local-planets commander)))

(defun travel (p)
  (interactive (list (completing-read "Planet Name: " (list-local-planets commander))))
  (move-to-planet commander (planet-name->planet p)))

(defun buy (tradegood num)
  (interactive "sTradegood: \nnAmount: ")
  (insert tradegood num))

;; Command Components ;;;;;;;;;;;;;;;;;;;
;;Info functions
(defun planet-info (p)
  (format "--==[ %s ]==--\n%s\nSize: % 10s\nPopulation: %s\nGovernment: %s\nTech-level: %s"
	  (planet-name p) (planet-description p) (planet-radius p) (planet-population p) 
	  (planet-government p) (planet-tech-level p)))

(defun captain-info (cmdr)
  (format "--==[ %s ]==--\nCredits: %s\nReputation: %s\nXP: %s\nCurrent Planet: %s\nShip: %s\n"
	  (captain-name cmdr) (captain-credits cmdr) (captain-reputation cmdr) (captain-xp cmdr) (captain-current-planet cmdr) (ship-name (captain-ship cmdr))))
  
(defun inventory (s)
  (let ((cargo (ship-cargo s))
	(fuel (ship-fuel s)))
    (format "%s\n%s" 
	    (if cargo
		(mapcar (lambda (i) (format "%s" i)) cargo)
	      (format "%s has nothing in her hold at the moment." (ship-name s)))
	    (if (> fuel 0)
		(format "Fuel Cells: %s/%s" fuel (ship-fuel-cap s))
	      (format "%s has nothing left in her fuel cells. Bust out the distress beacon, or abandon ship."  
		      (ship-name s))))))

(defun market-info (m)
  (mapcar (lambda (single-good)
	    (format "--==[ %s ]==--\nIn Stock: %s\nPrice/unit: %s\n\n" (car single-good) (cadr single-good) (caddr single-good)))
	  m))

(defun list-local-planets (cmdr)
  (mapcar (lambda (p) (planet-name p))
	  (systems-in-range (/ (ship-fuel (captain-ship commander)) 
			       (ship-fuel-consumption (captain-ship commander)))
			    (planet-name->planet (captain-current-planet commander)))))

(defun systems-in-range (a-range p)
  "Returns a list of planets within [a-range] of planet [p]"
  (filter (lambda (other-planet)
	    (> a-range (planet-distance p other-planet)))
	  galaxy))

(defun planet-distance (p1 p2)
  "Given two planets, returns the distance between them"
  (flet ((diff-sq (n1 n2) (* (- n1 n2) (- n1 n2))))
    (sqrt (+ (diff-sq (planet-z p1) (planet-z p2))
	     (diff-sq (planet-y p1) (planet-y p2))
	     (diff-sq (planet-x p1) (planet-x p2))))))

(defun move-to-planet (cmdr p)
  (let* ((fuel (ship-fuel (captain-ship cmdr)))
	 (current-planet (planet-name->planet (captain-current-planet cmdr)))
	 (distance (planet-distance current-planet p))
	 (fuel-range (/ fuel (ship-fuel-consumption (captain-ship cmdr)))))
    (if (>= fuel-range distance)
	(setf (captain-current-planet cmdr) (planet-name p)
	      (ship-fuel (captain-ship cmdr)) (round (- fuel (* distance (ship-fuel-consumption (captain-ship cmdr))))))
      (error "Planet out of range"))))

;;Actions
(defun buy (num good))
(defun sell (num good))

;; Getters
(defun planet-name->planet (p-name)
  "Given a planet name, returns that planets' struct (or nil if the planet doesn't exist in the game)"
  (find-if (lambda (p) (string= (planet-name p) p-name)) galaxy))

(provide 'leet)