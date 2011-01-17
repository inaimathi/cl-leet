(require 'leet-primitives)
(require 'leet-data)

;; Commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These are all side-effect functions, but don't have the bang because they may require the user to type them out
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
  (move-to-planet! commander (planet-name->planet p)))

(defun buy (t-name num)
  (interactive "sTradegood: \nnAmount: ")
  (purchase! commander t-name num))

(defun sell (t-name num)
  (interactive "sTradegood: \nnAmount: ")
  (convey! commander t-name num))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Actions ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun move-to-planet! (cmdr p)
  "Takes a commander and a planet, and moves the commander to the planet if its within rangex"
  (let* ((fuel (ship-fuel (captain-ship cmdr)))
	 (current-planet (planet-name->planet (captain-current-planet cmdr)))
	 (distance (planet-distance current-planet p))
	 (fuel-range (/ fuel (ship-fuel-consumption (captain-ship cmdr)))))
    (if (>= fuel-range distance)
	(setf (captain-current-planet cmdr) (planet-name p)
	      (ship-fuel (captain-ship cmdr)) (round (- fuel (* distance (ship-fuel-consumption (captain-ship cmdr))))))
      (error "Planet out of range"))))

(defun purchase! (cmdr t-name num)
  "Check if a purchase order is valid, and if so, fulfill it"
  (let ((good (tradegood-available? t-name (planet-market (planet-name->planet (captain-current-planet cmdr))))))
    (cond ((not good) (error "That's not available at this planet"))
	  ((< (cadr good) num) (error (format "They don't have that many %s" t-name)))
	  ((< (captain-credits cmdr) (* num (caddr good))) (error (format "You can't afford that many %s" t-name)))
	  ((not (enough-space? cmdr t-name num)) (error "You don't have enough room in your cargo hold"))
	  (t (setf (cadr good) (- (cadr good) num) ;; Remoe [num] [t-name] from the planet
		   (captain-credits cmdr) (- (captain-credits cmdr) (* num (caddr good)))) ;; Remove (* [num] [price]) credits from captains' account
	     (add-to-inventory! cmdr t-name num)
	     (format "Bought %s %s" num t-name)))))

(defun convey! (cmdr t-name num)
  "Check if a sell order is valid, and if so, fulfill it"
  (let ((sell-price (going-rate (captain-current-planet cmdr) t-name))
	(inventory-listing (assoc (capitalize t-name) (ship-cargo (captain-ship cmdr)))))
    (cond ((not sell-price) (error (format "I have no clue what a %s is" t-name)))
	  ((not inventory-listing) (error (format "You don't have any %s in your hold" t-name)))
	  ((> num (cadr inventory-listing)) (error (format "You don't have enough %s in your hold" t-name)))
	  (t (remove-from-inventory! cmdr t-name num)
	     (add-to-market! (captain-current-planet cmdr) t-name num)
	     (setf (captain-credits cmdr) (+ (captain-credits cmdr) (* sell-price num)))))))
;; sell (remove goods from hold, add them to the planet inventory, add credits to captain account)

(defun add-to-market! (p-name t-name num)
  "Add [num] [t-good] to [p-name]s market"
  (let* ((market (planet-market (planet-name->planet p-name)))
	 (listing (tradegood-available? t-name market)))
    (if listing
	(setf (cadr listing) (+ (cadr listing) num))
      (setf market (cons (list (capitalize t-name) num (going-rate p-name t-name)) market)))))

(defun add-to-inventory! (cmdr t-name num)
  "Add [num] [t-good] to [cmdr]s inventory"
  (let ((listing (assoc (capitalize t-name) (ship-cargo (captain-ship cmdr))))
	(ship (captain-ship cmdr))
	(good (tradegood-name->tradegood t-name)))
    (cond ((and (fuel? good) (> (ship-fuel-space ship) 0)); Fill out fuel-cells before filling out cargo hold if there's space
	   (let ((f-space (ship-fuel-space ship)))
	     (if (>= f-space num)
		 (setf (ship-fuel ship) (+ (ship-fuel ship) num))
	       (progn (setf (ship-fuel ship) (ship-fuel-cap ship))
		      (add-to-inventory! cmdr t-name (- num f-space))))))
	  (listing (setf (cadr listing) (+ (cadr listing) num))) ;; If there's already some [good] in inventory, just add it to the pile
	  (t (setf (ship-cargo (captain-ship cmdr))
		   (cons (list (capitalize t-name) num) (ship-cargo (captain-ship cmdr)))))))) ;; otherwise add a new entry

(defun remove-from-inventory! (cmdr t-name num)
  "Remove [num] [t-good] from [cmdr]s inventory"
  (let* ((cargo (ship-cargo (captain-ship cmdr)))
	 (listing (assoc (capitalize t-name) cargo)))
    (if (= (cadr listing) num)
	(setf (ship-cargo (captain-ship cmdr)) (remove-if (lambda (l) (string= (capitalize t-name) (car l))) cargo))
      (setf (cadr listing) (- (cadr listing) num)))))

(defun record-trade-history! (cmdr trade)
  (setf (captain-trade-history cmdr)
	(cons trade (captain-trade-history cmdr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Oddly Specific Predicates;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun enough-space? (cmdr t-name num)
  "Takes a [captain], [tradegood-name] and [amount]. Returns true if there is enough room for [amount] [tradegood-name] in [captain]s' ship."
  (let ((g (tradegood-name->tradegood t-name))
	(c-space (ship-cargo-space (captain-ship cmdr)))
	(f-space (ship-fuel-space (captain-ship cmdr))))
    (if (fuel? g)
	(or (>= c-space num) (>= f-space num) (>= (+ c-space f-space) num))
      (>= c-space num))))

(defun fuel? (g)
  "Returns true if [t] is a tradegood of type 'fuel"
  (and (tradegood-p g)
       (eq (tradegood-type g) 'fuel)))

(defun tradegood-available? (t-name market)
  "Takes a tradegood name and a market, returns that tradegoods stats on the market (nil if it is unavailable)"
  (assoc (capitalize t-name) market))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additional Getters ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun going-rate (p-name t-name)
  "Given a planet name and tradegood name, returns the price/unit of tradegood on planet"
  (let* ((plt (planet-name->planet p-name))
	 (good (tradegood-name->tradegood t-name))
	 (listing (tradegood-available? t-name (planet-market plt))))
    (cond (listing (caddr listing))
	  ((and (not listing) good) 300) ;; TODO: calculate going rate based on planet-tech-level/item-tech-level/item-base-price
	  (t nil)))) ;; If we've gotten here, it means that the tradegood given doesn't exist in game

(defun planet-info (p)
  (format "--==[ %s ]==--\n%s\nSize: % 10s\nPopulation: %s\nGovernment: %s\nTech-level: %s\n\n"
	  (planet-name p) (planet-description p) (planet-radius p) (planet-population p) 
	  (planet-government p) (planet-tech-level p)))

(defun captain-info (cmdr)
  (format "--==[ %s ]==--\nCredits: %s\nReputation: %s\nXP: %s\nCurrent Planet: %s\nShip: %s\n\n"
	  (captain-name cmdr) (captain-credits cmdr) (captain-reputation cmdr) (captain-xp cmdr) (captain-current-planet cmdr) (ship-name (captain-ship cmdr))))
  
(defun inventory (s)
  "Takes a ship and outputs the contents of its cargo bay"
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
  "Takes a market and prints all goods available on it"
  (mapcar (lambda (single-good)
	    (format "--[ %s ]--\nIn Stock: %s\nPrice/unit: %s\n\n" 
		    (car single-good) (cadr single-good) (caddr single-good)))
	  m))

(defun list-local-planets (cmdr)
  "Takes a commander and outputs all directly reachable planets given their ships fuel and fuel-consumption"
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

(defun planet-name->planet (p-name)
  "Given a planet name, returns that planets' struct (or nil if the planet doesn't exist in the game)"
  (find-if (lambda (p) (string= (planet-name p) p-name)) galaxy))

(defun tradegood-name->tradegood (t-name)
  "Given a tradegood name, returns that tradegoods' struct (or nil if it doesn't exist in the game)"
  (find-if (lambda (g) (string= (tradegood-name g) (capitalize t-name))) tradegoods))

(defun ship-cargo-space (s)
  "Returns amount of free cargo space in the given ship"
  (- (ship-cargo-cap s)
     (apply '+ (mapcar (lambda (g) (or (cadr g) 0)) (ship-cargo s)))))

(defun ship-fuel-space (s)
  "Returns amount of free fuel space in the given ship"
  (- (ship-fuel-cap s) (ship-fuel s)))

(provide 'leet)