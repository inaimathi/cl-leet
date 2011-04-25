(in-package :cl-leet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Structs
(defstruct tradegood price tech-level complement-type name unit type)
(defstruct listing name amount price)

(defstruct planet id name description 
	   radius x y z 
	   tech-level productivity market
	   local-goods banned-goods 
	   (:copier 'copy-planet))

(defun copy-planet (p)
  (let ((a-copy (copy-structure p)))
    (setf (planet-market a-copy) (mapcar #'copy-structure (planet-market p)))
    a-copy))

(defstruct captain ship credits current-planet trade-history)
(defstruct ship fuel-consumption fuel-cap fuel cargo-cap cargo)

(defstruct trade-record planet good amount price/unit type)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Queries
;;;;;;;;;; Selects
(defun list-local-planets (a-cap)
  "Takes a captain and returns all directly reachable planets given their ships fuel and fuel-consumption"
  (mapcar (lambda (p) (planet-name p))
	  (planets-in-range (/ (ship-fuel (captain-ship a-cap)) 
			       (ship-fuel-consumption (captain-ship a-cap)))
			    (planet-name->planet (captain-current-planet a-cap)))))

(defun list-galaxy () *galaxy*)

;;;;;;;;;; Inserts/Updates
(defun record-trade-history! (a-cap type planet amount t-name price/unit)
  (let ((trade (make-trade-record 
		:type type :planet planet
		:amount amount :good t-name :price/unit price/unit)))
    (setf (captain-trade-history a-cap) (cons trade (captain-trade-history a-cap)))))

(defun add-to-market! (p-name t-name num sell-price)
  "Add [num] [t-good] to [p-name]s market"
  (let* ((market (planet-market (planet-name->planet p-name)))
	 (a-listing (tradegood-available? t-name market)))
    (if a-listing
	(setf (listing-amount a-listing) (+ (listing-amount a-listing) num))
	(progn (setf (planet-market (planet-name->planet p-name)) (cons (make-listing :name (string-capitalize t-name) :amount num :price sell-price) market))
	       (incf (planet-tech-level (planet-name->planet p-name)) (roll-dice 1 4))))))

(defun add-to-cargo! (a-cap t-name num price)
  "Add [num] [t-good] to [a-cap]s inventory"
  (let ((a-listing (tradegood-available? t-name (ship-cargo (captain-ship a-cap))))
	(ship (captain-ship a-cap))
	(good (tradegood-name->tradegood t-name)))
    (cond ((and (fuel? good) (> (ship-fuel-space ship) 0)); Fill out fuel-cells before filling out cargo hold if there's space
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
	 (current-planet (planet-name->planet (captain-current-planet a-cap)))
	 (distance (planet-distance current-planet p)))
    (setf (captain-current-planet a-cap) (planet-name p)
	  (ship-fuel (captain-ship a-cap)) (- fuel (round (* distance (ship-fuel-consumption (captain-ship a-cap))))))))

(defun process-purchase! (a-cap a-listing num)
    (let ((t-name (listing-name a-listing)))
    (setf (listing-amount a-listing) (- (listing-amount a-listing) num) ;; Remove [num] [t-name] from the planet
	  (captain-credits a-cap) (- (captain-credits a-cap) (* num (listing-price a-listing)))) ;; Remove (* [num] [price]) credits from captains' account
    (add-to-cargo! a-cap t-name num (listing-price a-listing))
    (record-trade-history! a-cap 'buy (captain-current-planet a-cap) num (string-capitalize t-name) (listing-price a-listing))
    (format nil "Bought ~a ~a" num t-name)))

(defun process-sale! (a-cap a-listing sell-price num)
  (let ((t-name (listing-name a-listing)))
    (remove-from-cargo! a-cap t-name num)
    (add-to-market! (captain-current-planet a-cap) t-name num sell-price)
    (setf (captain-credits a-cap) (+ (captain-credits a-cap) (* sell-price num)))
    (record-trade-history! a-cap 'sell (captain-current-planet a-cap) num (string-capitalize t-name) (listing-price a-listing))
    (format nil "Sold ~a ~a" num t-name)))

(defun remove-from-cargo! (a-cap t-name num)
  "Remove [num] [t-good] from [a-cap]s inventory"
  (let* ((cargo (ship-cargo (captain-ship a-cap)))
	 (a-listing (tradegood-available? t-name cargo)))
    (if (= (listing-amount a-listing) num)
	(setf (ship-cargo (captain-ship a-cap))
	      (remove-if (lambda (l) (string= (string-capitalize t-name) (listing-name l))) cargo))
	(setf (listing-amount a-listing) (- (listing-amount a-listing) num)))))

(defun galaxy-produce! ()
  (dolist (a-planet *galaxy*)
    (market-produce! (planet-productivity a-planet) (planet-market a-planet))))

(defun market-produce! (productivity a-market)
  (dolist (l a-market)
    (let* ((g (tradegood-name->tradegood (listing-name l)))
	   (tech-level (max 1 (tradegood-tech-level g)))
	   (produced (+ (roll-dice 2 20) (round (/ productivity tech-level))))
	   (new-price (mean (apply #'roll-dice (tradegood-price g)) (listing-price l))))
      (setf (listing-amount l) (+ (listing-amount l) (if (fuel? g) (* 2 produced) produced))
	    (listing-price l) new-price))))
	      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Oddly Specific Predicates;;
(defun enough-space? (a-cap t-name num)
  "Takes a [captain], [tradegood-name] and [amount]. Returns true if there is enough room for [amount] [tradegood-name] in [captain]s' ship."
  (let ((g (tradegood-name->tradegood t-name))
	(c-space (ship-cargo-space (captain-ship a-cap)))
	(f-space (ship-fuel-space (captain-ship a-cap))))
    (if (fuel? g)
	(or (>= c-space num) (>= f-space num) (>= (+ c-space f-space) num))
      (>= c-space num))))

(defun fuel? (g)
  "Returns true if [t] is a tradegood of type 'fuel"
  (and (tradegood-p g) (eq (tradegood-type g) 'fuel)))

(defun tradegood-available? (t-name inv)
  "Takes a tradegood name and an inventory, returns that tradegoods stats in that inventory (nil if it is unavailable)"
  (let ((n (string-capitalize t-name)))
    (find-if (lambda (l) (string= n (listing-name l))) inv)))

(defun within-distance? (a-cap p)
  (let* ((fuel (ship-fuel (captain-ship a-cap)))
	 (current-planet (planet-name->planet (captain-current-planet a-cap)))
	 (distance (planet-distance current-planet p))
	 (fuel-range (/ fuel (ship-fuel-consumption (captain-ship a-cap)))))
    (>= fuel-range distance)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Additional Getters
(defun going-rate (p-name t-name)
  "Given a planet name and tradegood name, returns the price/unit of tradegood on planet"
  (let* ((plt (planet-name->planet p-name))
	 (good (tradegood-name->tradegood t-name))
	 (a-listing (tradegood-available? t-name (planet-market plt))))
    (cond ((listing-p a-listing) (listing-price a-listing)) ;; The good is on the market here; use the latest generated price for it
	  ((and (not a-listing) good) (* (roll-dice 2 3) (apply #'roll-dice (tradegood-price good)))) ;; The tradegood is not on the market here (sell for 2d3x going rate)
	  (t nil)))) ;; tradegood given doesn't exist in game  

(defun planet-listing (a-cap t-name)
  "Returns the listing for [t-name] on [a-cap]s' current planet. NIL if it isn't sold there."
  (tradegood-available? "Fuel" (planet-market (planet-name->planet (captain-current-planet a-cap)))))

(defun planets-in-range (a-range p)
  "Returns a list of planets within [a-range] of planet [p]"
  (remove-if (lambda (other-planet) (equalp p other-planet))
	     (remove-if-not (lambda (other-planet)
			      (> a-range (planet-distance p other-planet)))
			    *galaxy*)))

(defun planet-fuel-cost (a-cap p)
  "Returns the amount of fuel [a-cap] would have to burn to reach [p]"
  (let ((distance (planet-distance p (planet-name->planet (captain-current-planet a-cap)))))
    (round (* distance (ship-fuel-consumption (captain-ship a-cap))))))

(defun planet-distance (p1 p2)
  "Given two planets, returns the euclidean distance between them"
  (flet ((diff-sq (n1 n2) (expt (- n1 n2) 2)))
    (sqrt (+ (diff-sq (planet-z p1) (planet-z p2))
	     (diff-sq (planet-y p1) (planet-y p2))
	     (diff-sq (planet-x p1) (planet-x p2))))))

(defun planet-name->planet (p-name)
  "Given a planet name, returns that planets' struct (or nil if the planet doesn't exist in the game)"
  (find-if (lambda (p) (string= (planet-name p) p-name)) *galaxy*))

(defun tradegood-name->tradegood (t-name)
  "Given a tradegood name, returns that tradegoods' struct (or nil if it doesn't exist in the game)"
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
(defun generate-captain () ;;kind of a misnomer at the moment, but I need that level of indirection in case I change the representation
  (make-captain :credits 10000
		:current-planet (planet-name (car *galaxy*))
		:trade-history nil
		:ship (make-ship :cargo-cap 50
				 :cargo nil
				 :fuel-consumption 1
				 :fuel-cap 300
				 :fuel 300)))

(defun generate-planet (plan-id)
  (let* ((rad (roll-dice 4 12))
	 (tech (roll-dice 3 4))
	 (prod (round (roll-dice 2 6 (+ rad tech)))))
    (make-planet :id plan-id :name (string-capitalize (grammar->string *planet-name-grammar*))
		 :description (grammar->string *planet-desc-grammar*)
		 :radius rad :x (random 500) :y (random 500) :z (random 500)
		 :market (generate-market rad tech prod)
		 :tech-level tech
		 :productivity prod)))

(defun generate-market (rad tech prod)
  (let ((possible-goods (remove-if-not (lambda (g) (>= tech (tradegood-tech-level g))) *tradegoods*)))
    (mapcar (lambda (g)
	      (let* ((amt (round (max 0 (/ (* prod tech) (max 1 (tradegood-tech-level g))))))
		     (pri (round (apply #'roll-dice (tradegood-price g)))))
		(make-listing :name (tradegood-name g) :amount amt :price pri)))
	    possible-goods)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Grammar functions
(defun pick-g (key grammar) 
  (let ((choices (getf grammar key)))
    (nth (random (length choices)) choices)))

(defun grammar->string (grammar) (expand-production :root grammar))

(defun expand-production (production grammar)
  (cond ((stringp production) production)
	((symbolp production) (expand-production (pick-g production grammar) grammar))
	((listp production) 
	 (reduce (lambda (a b) 
		   (concatenate 'string a (expand-production b grammar))) 
		 (cons "" production)))))