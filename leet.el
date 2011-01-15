(require 'leet-primitives)
(require 'leet-data)

;; Commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cap-info () 
  (interactive)
  (insert (captain-info commander)))

(defun plt-info ()
  (interactive)
  (insert (planet-info (planet-name->planet (captain-current-planet commander)))))

(defun market ()
  (interactive)
  (mapcar 'insert 
	  (market-info (planet-market (planet-name->planet (captain-current-planet commander))))))

(defun cargo ()
  (interactive)
  (insert (inventory (captain-ship commander))))

(defun local-planets ()
  (interactive)
  (mapcar (lambda (p) (insert p "\n"))
	  (list-local-planets commander)))

(defun travel (p)
  (interactive (list (completing-read "Planet Name: " (list-local-planets commander))))
  (move-to-planet commander (planet-name->planet p)))

(defun buy (tradegood num)
  (interactive "sTradegood: \nnAmount: ")
  (purchase num tradegood commander))
  ;;(insert (format "%s %s\n\n" tradegood num)))

;; Command Components ;;;;;;;;;;;;;;;;;;;
;;Info functions
(defun planet-info (p)
  (format "--==[ %s ]==--\n%s\nSize: % 10s\nPopulation: %s\nGovernment: %s\nTech-level: %s\n\n"
	  (planet-name p) (planet-description p) (planet-radius p) (planet-population p) 
	  (planet-government p) (planet-tech-level p)))

(defun captain-info (cmdr)
  (format "--==[ %s ]==--\nCredits: %s\nReputation: %s\nXP: %s\nCurrent Planet: %s\nShip: %s\n\n"
	  (captain-name cmdr) (captain-credits cmdr) (captain-reputation cmdr) (captain-xp cmdr) (captain-current-planet cmdr) (ship-name (captain-ship cmdr))))
  
(defun inventory (s)
  (let ((cargo (ship-cargo s))
	(fuel (ship-fuel s)))
    (format "%s\n%s\n\n" 
	    (if cargo
		(mapcar (lambda (i) (format "%s" i)) cargo)
	      (format "%s has nothing in her hold at the moment." (ship-name s)))
	    (if (> fuel 0)
		(format "Fuel Cells: %s/%s" fuel (ship-fuel-cap s))
	      (format "%s has nothing left in her fuel cells. Bust out the distress beacon, or abandon ship."  
		      (ship-name s))))))

(defun market-info (m)
  (mapcar (lambda (single-good)
	    (format "--[ %s ]--\nIn Stock: %s\nPrice/unit: %s\n\n" 
		    (car single-good) (cadr single-good) (caddr single-good)))
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
(defun purchase (num tradegood cmdr)
  (let ((good (tradegood-available? tradegood (planet-market (planet-name->planet (captain-current-planet cmdr))))))
    (cond ((not good) (error "That's not available at this planet"))
	  ((< (cadr good) num) (error (format "They don't have that many %s" tradegood)))
	  ((< (captain-credits cmdr) (* num (caddr good))) (error "You can't afford that many."))
	  ((< (ship-cargo-space (captain-ship cmdr)) num) (error "You don't have enough room in your cargo hold"))
	  (t (setf (cadr good) (- (cadr good) num) ;; Remoe [num] [tradegood] from the planet
		   (captain-credits cmdr) (- (captain-credits cmdr) (* num (caddr good))))
	     (add-to-inventory cmdr tradegood num)))))
	  ;;This involves (in order):
	  ;; - Remove [num] [tradegood] from (tradegood-available? tradegood (planet-market (captain-current-planet cmdr)))
	  ;; - Add [num] [tradegood] to the captains' hold (or fuel-tank/hold if it's a fuel)
	  ;; - Remove (* num (caddr good)) credits from the captains' account

(defun add-to-inventory (cmdr tradegood num)
  (let ((g (assoc (capitalize tradegood) (ship-cargo (captain-ship cmdr)))))
    (if g
	(setf (cadr g) (+ (cadr g) num))
      (setf (ship-cargo (captain-ship cmdr))
	    (cons (list (capitalize tradegood) num) (ship-cargo (captain-ship cmdr)))))))
	
(defun ship-cargo-space (s)
  (- (ship-cargo-cap s)
     (apply '+ (mapcar (lambda (g) (or (cadr g) 0)) (ship-cargo s)))))

(defun tradegood-available? (tradegood market)
  "Takes a tradegood name, returns that tradegoods stats on the current market (or NIL if it is unavailable)"
  (assoc (capitalize tradegood) market))

(defun sell (num good))

;; Getters
(defun planet-name->planet (p-name)
  "Given a planet name, returns that planets' struct (or nil if the planet doesn't exist in the game)"
  (find-if (lambda (p) (string= (planet-name p) p-name)) galaxy))

(defun tradegood-name->tradegood (t-name)
  (find-if (lambda (g) (string= (tradegood-name g))) tradegoods))

(provide 'leet)