(in-package :cl-leet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Structs
(defstruct tradegood price tech-level complement-type name unit type)
(defstruct listing name amount price tech-level)

(defstruct planet id name description 
	   radius x y z 
	   tech-level productivity market
	   local-goods banned-goods 
	   (:copier 'copy-planet))

(defun copy-planet (p)
  (let ((a-copy (copy-structure p)))
    (setf (planet-market a-copy) (mapcar #'copy-structure (planet-market p)))
    a-copy))

(defstruct captain ship credits current-planet trade-history transaction)
(defstruct ship fuel-consumption fuel-cap fuel cargo-cap cargo)

(defstruct trade planet good amount price/unit type)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Queries
;;;;;;;;;; Selects
(defun list-local-planets (a-cap)
  "Takes a captain and returns all directly reachable planets given their ships fuel and fuel-consumption"
  (mapcar (lambda (p) (planet-name p))
	  (planets-in-range (/ (ship-fuel (captain-ship a-cap)) 
			       (ship-fuel-consumption (captain-ship a-cap)))
			    (captain-current-planet a-cap))))

(defun list-galaxy () *galaxy*)

;;;;;;;;;; Inserts/Updates
(defun commit-transactions! (a-cap)
  "Takes a captain and applies their latest transactions to the global record"
  (let ((ts (captain-transaction a-cap))
	(p (lookup-planet (planet-name (captain-current-planet a-cap)))))
    (when ts (loop for (a-listing amt-change) in ts
		do (add-to-market! p (listing-name a-listing) (listing-price a-listing) amt-change)))
    (setf (captain-transaction a-cap) nil)))

(defun trade-change (type amount)
  (case type
    ('sell amount)
    ('buy (- amount))
    (otherwise (error (format nil "trade-change: Invalid trade record type - ~a" a-trade)))))

(defun record-trade! (a-cap type amount a-listing)
  "Records an action to trade history and to the current transaction"
  (let ((change (trade-change type amount))
	(a-trade (make-trade 
		  :type type :planet (captain-current-planet a-cap)
		  :amount amount :good (listing-name a-listing) :price/unit (listing-price a-listing))))
    (setf (captain-trade-history a-cap) (cons a-trade (captain-trade-history a-cap))
	  (captain-transaction a-cap) (cons (list a-listing change) (captain-transaction a-cap)))))

(defun add-to-market! (a-planet t-name num sell-price)
  "Add [num] [t-good] to [a-planet]s market"
  (let* ((market (planet-market a-planet))
	 (a-listing (lookup-listing t-name market)))
    (if a-listing
	(setf (listing-amount a-listing) (+ (listing-amount a-listing) num))
	(let ((g (lookup-tradegood t-name)))
	  (progn (setf (planet-market a-planet) 
		       (sort (cons (make-listing :name (tradegood-name g) :amount num :price sell-price :tech-level (tradegood-tech-level g)) market)
			     (lambda (a b) (> (listing-tech-level a) (listing-tech-level b)))))
		 (incf (planet-tech-level a-planet) (roll-dice 1 4)))))))

(defun add-to-cargo! (a-cap t-name num price)
  "Add [num] [t-good] to [a-cap]s inventory"
  (let ((a-listing (lookup-listing t-name (ship-cargo (captain-ship a-cap))))
	(ship (captain-ship a-cap))
	(g (lookup-tradegood t-name)))
    (cond ((and (fuel? g) (> (ship-fuel-space ship) 0)); Fill out fuel-cells before filling out cargo hold if there's space
	   (let ((f-space (ship-fuel-space ship)))
	     (if (>= f-space num)
		 (setf (ship-fuel ship) (+ (ship-fuel ship) num))
		 (progn (setf (ship-fuel ship) (ship-fuel-cap ship))
			(add-to-cargo! a-cap t-name (- num f-space))))))
	  (a-listing (setf (listing-amount a-listing) (+ (listing-amount a-listing) num)))
	  (t (setf (ship-cargo (captain-ship a-cap))
		   (cons (make-listing :name (string-capitalize t-name) :amount num :price price) (ship-cargo (captain-ship a-cap)))))))) ;; otherwise add a new entry

;;;;;;;;;; Updates/Deletes
(defun move-captain! (a-cap p)
  (let* ((fuel (ship-fuel (captain-ship a-cap)))
	 (current-planet (captain-current-planet a-cap))
	 (distance (planet-distance current-planet p)))
    (commit-transactions! a-cap)
    (setf (captain-current-planet a-cap) (copy-planet p)
	  (ship-fuel (captain-ship a-cap)) (- fuel (round (* distance (ship-fuel-consumption (captain-ship a-cap))))))))

(defun process-purchase! (a-cap a-listing num)
    (let ((t-name (listing-name a-listing)))
      (setf (listing-amount a-listing) (- (listing-amount a-listing) num) ;; Remove [num] [t-name] from the planet
	    (captain-credits a-cap) (- (captain-credits a-cap) (* num (listing-price a-listing)))) ;; Remove (* [num] [price]) credits from captains' account
      (add-to-cargo! a-cap t-name num (listing-price a-listing))
      (record-trade! a-cap 'buy num a-listing)))

(defun process-sale! (a-cap a-listing sell-price num)
  (let ((t-name (listing-name a-listing)))
    (remove-from-cargo! a-cap t-name num)
    (add-to-market! (captain-current-planet a-cap) t-name num sell-price)
    (setf (captain-credits a-cap) (+ (captain-credits a-cap) (* sell-price num)))
    (record-trade! a-cap 'sell num a-listing)))

(defun remove-from-cargo! (a-cap t-name num)
  "Remove [num] [t-good] from [a-cap]s inventory"
  (let* ((cargo (ship-cargo (captain-ship a-cap)))
	 (a-listing (lookup-listing t-name cargo)))
    (if (= (listing-amount a-listing) num)
	(setf (ship-cargo (captain-ship a-cap))
	      (remove-if (lambda (l) (string= (string-capitalize t-name) (listing-name l))) cargo))
	(setf (listing-amount a-listing) (- (listing-amount a-listing) num)))))

(defun galaxy-produce! ()
  (dolist (a-planet *galaxy*)
    (planet-produce! a-planet)))

(defun planet-produce! (a-planet)
  (dolist (l (planet-market a-planet))
    (let* ((g (lookup-tradegood (listing-name l)))
	   (tech-level (max 1 (tradegood-tech-level g)))
	   (produced (+ (roll-dice 2 20) (round (/ (planet-productivity a-planet) tech-level))))
	   (new-price (mean (generate-price g a-planet) (listing-price l))))
      (setf (listing-amount l) (+ (listing-amount l) (if (fuel? g) (* 2 produced) produced))
	    (listing-price l) new-price))))
	      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Oddly Specific Predicates;;
(defun enough-space? (a-cap t-name num)
  "Takes a [captain], [tradegood-name] and [amount]. Returns true if there is enough room for [amount] [tradegood-name] in [captain]s' ship."
  (let ((g (lookup-tradegood t-name))
	(c-space (ship-cargo-space (captain-ship a-cap)))
	(f-space (ship-fuel-space (captain-ship a-cap))))
    (if (fuel? g)
	(or (>= c-space num) (>= f-space num) (>= (+ c-space f-space) num))
      (>= c-space num))))

(defun fuel? (g)
  "Returns true if [t] is a tradegood of type 'fuel"
  (and (tradegood-p g) (eq (tradegood-type g) 'fuel)))

(defun within-distance? (a-cap p)
  (let* ((fuel (ship-fuel (captain-ship a-cap)))
	 (current-planet (captain-current-planet a-cap))
	 (distance (planet-distance current-planet p))
	 (fuel-range (/ fuel (ship-fuel-consumption (captain-ship a-cap)))))
    (>= fuel-range distance)))

(defun banned? (t-name a-planet) (find t-name (planet-banned-goods a-planet)))
(defun local? (t-name a-planet) (find t-name (planet-local-goods a-planet)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Additional Getters
(defun going-rate (a-planet t-name)
  "Given a planet name and tradegood name, returns the price/unit of tradegood on planet"
  (let* ((good (lookup-tradegood t-name))
	 (a-listing (lookup-listing t-name (planet-market a-planet))))
    (cond ((listing-p a-listing) (listing-price a-listing)) ;; The good is on the market here; use the latest generated price for it
	  ((and (not a-listing) good) (* (roll-dice 2 3) (apply #'roll-dice (tradegood-price good)))) ;; The tradegood is not on the market here (sell for 2d3x going rate)
	  (t (error (format nil "going-rate: invalid tradegood -- ~a" t-name))))))

(defun planets-in-range (a-range p)
  "Returns a list of planets within [a-range] of planet [p]"
  (remove-if (lambda (other-planet) (equalp p other-planet))
	     (remove-if-not (lambda (other-planet)
			      (> a-range (planet-distance p other-planet)))
			    *galaxy*)))

(defun planet-fuel-cost (a-cap p)
  "Returns the amount of fuel [a-cap] would have to burn to reach [p]"
  (let ((distance (planet-distance p (captain-current-planet a-cap))))
    (round (* distance (ship-fuel-consumption (captain-ship a-cap))))))

(defun planet-distance (p1 p2)
  "Given two planets, returns the euclidean distance between them"
  (flet ((diff-sq (n1 n2) (expt (- n1 n2) 2)))
    (sqrt (+ (diff-sq (planet-z p1) (planet-z p2))
	     (diff-sq (planet-y p1) (planet-y p2))
	     (diff-sq (planet-x p1) (planet-x p2))))))

(defun lookup-planet (p-name)
  "Look up [p-name] (nil if the planet doesn't exist in *galaxy*)"
  (find-if (lambda (p) (string= (planet-name p) p-name)) *galaxy*))

(defun lookup-listing (t-name inventory)
  "Looks up [t-name] in [inventory] (nil if it is unavailable there). Inventory can refer to any list of listings (both markets and cargos are currently represented this way)"
  (let ((n (string-capitalize t-name)))
    (find-if (lambda (l) (string= n (listing-name l))) inventory)))

(defun lookup-tradegood (t-name)
  "Looks up [t-name] in the global tradegoods table (or nil if it doesn't exist in the game). It doesn't accept a structure to search through because there's only one place you could be looking for tradegood structs (all others use listings)"
  (find-if (lambda (g) (string= (tradegood-name g) (string-capitalize t-name))) *tradegoods*))

(defun ship-cargo-space (s)
  "Returns amount of free cargo space in the given ship"
  (- (ship-cargo-cap s) (ship-cargo-total s)))

(defun ship-cargo-total (s)
  "Returns amount of used space in the given ship"
  (apply '+ (mapcar (lambda (a-listing) (listing-amount a-listing)) (ship-cargo s))))

(defun ship-fuel-space (s)
  "Returns amount of free fuel space in the cells of a given ship"
  (- (ship-fuel-cap s) (ship-fuel s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Generators
(defun generate-captain ()
  (make-captain :credits 10000
		:current-planet (copy-planet (car *galaxy*))
		:trade-history nil
		:ship (make-ship :cargo-cap 50
				 :cargo nil
				 :fuel-consumption 1
				 :fuel-cap 300
				 :fuel 300)))

(defun generate-galaxy (num-planets)
  (loop for i from 1 to num-planets
     collect (generate-planet i 1500)))

(defun partition-galaxy (a-galaxy &optional (step 100))
  (loop for d from step to 1600 by step
     collect (remove-if-not (lambda (p) 
			      (and (< (planet-z p) d) (> (planet-z p) (- d step))))
			    a-galaxy)))

(defun generate-planet (plan-id &optional (galaxy-dimension 500))
  (flet ((unique-names (t-goods) (remove-duplicates (mapcar #'tradegood-name t-goods) :test #'string=)))
    (let* ((rad (roll-dice 4 12))
	   (tech (roll-dice 3 4))
	   (prod (round (roll-dice 2 6 (+ rad tech))))
	   (possible-goods (remove-if-not (lambda (g) (>= tech (tradegood-tech-level g))) *tradegoods*))
	   (local (unique-names (pick-n possible-goods (roll-dice 1 6 +1))))
	   (banned (unique-names (pick-n *tradegoods* (roll-dice 1 4 -1))))
	   (a-planet (make-planet :id plan-id :name (string-capitalize (grammar->string *planet-name-grammar*))
				  :description (grammar->string *planet-desc-grammar*)
				  :radius rad :x (random galaxy-dimension) :y (random galaxy-dimension) :z (random galaxy-dimension)
				  :banned-goods banned :local-goods local
				  :tech-level tech
				  :productivity prod)))
      (setf (planet-market a-planet) (generate-market a-planet possible-goods))
      a-planet)))

(defun generate-market (a-planet possible-goods) ;; rad tech prod banned local
  (mapcar (lambda (g)
	    (let* ((amt (round (max 0 (/ (* (planet-productivity a-planet) (planet-tech-level a-planet)) (max 1 (tradegood-tech-level g))))))
		   (pri (generate-price g a-planet)))
	      (make-listing :name (tradegood-name g) :amount amt :price pri :tech-level (tradegood-tech-level g))))
	  possible-goods))

(defun generate-price (a-tradegood a-planet)
  (let ((price (round (apply #'roll-dice (tradegood-price a-tradegood)))))
    (cond ((banned? (tradegood-name a-tradegood) a-planet) (+ (roll-dice 3 20 10) price))
	  ((local? (tradegood-name a-tradegood) a-planet) (round (/ price (planet-tech-level a-planet))))
	  (t price))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Grammar functions
(defun grammar-pick (key grammar) (pick (getf grammar key)))

(defun grammar->string (grammar) (expand-production :root grammar))

(defun expand-production (production grammar)
  (cond ((stringp production) production)
	((symbolp production) (expand-production (grammar-pick production grammar) grammar))
	((listp production) 
	 (reduce (lambda (a b) 
		   (concatenate 'string a (expand-production b grammar))) 
		 (cons "" production)))))