(in-package :cl-leet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Structs
(defstruct tradegood base-price tech-level complement-type name unit
	   type) ;; right now either 'goods 'fuel ('gear to be added later). This will be used as "substitute"
(defstruct listing name amount price)

(defstruct planet name description radius x y z tech-level productivity
	   market) ;; (list (:tradegood [tradegood] :price [price] :quantity [quantity]) ...)

(defstruct captain name ship credits current-planet trade-history)
(defstruct ship name frame engine speed fuel-consumption fuel-cap fuel cargo-cap cargo)

(defstruct trade-record planet good amount price/unit type) ;;'buy or 'sell

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Queries
;;;;;;;;;; Selects
(defun captain-info (a-cap)
  (list :name (captain-name a-cap)
	:credits (captain-credits a-cap)
	:current-planet (captain-current-planet a-cap)
	:ship-name (ship-name (captain-ship a-cap))))

(defun planet-info (p)
  (list :name (planet-name p)
	:description (planet-description p)
	:radius (planet-radius p)
	:tech-level (planet-tech-level p)))

(defun market-info (m)
  (mapcar (lambda (a-listing)
	    (list :name (listing-name a-listing)
		  :in-stock (listing-amount a-listing)
		  :price (listing-price a-listing)))
	  m))

(defun inventory (s)
  "Takes a ship and outputs the contents of its cargo bay"
  (let ((cargo (ship-cargo s))
	(fuel (ship-fuel s)))
    (list :cargo cargo
	  :fuel fuel
	  :fuel-cap (ship-fuel-cap s))))

(defun list-local-planets (a-cap)
  "Takes a captain and outputs all directly reachable planets given their ships fuel and fuel-consumption"
  (mapcar (lambda (p) (planet-name p))
	  (systems-in-range (/ (ship-fuel (captain-ship a-cap)) 
			       (ship-fuel-consumption (captain-ship a-cap)))
			    (planet-name->planet (captain-current-planet a-cap)))))

(defun list-galaxy () *galaxy*)
;;  (sort (copy-list *galaxy*) (lambda (a b) (> (planet-z a) (planet-z b)))))

;;;;;;;;;; Inserts/Updates
(defun record-trade-history! (a-cap type planet amount t-name price/unit)
  (let ((trade (make-trade-record 
		:type type :planet planet
		:amount amount :good t-name :price/unit price/unit)))
    (setf (captain-trade-history a-cap) (cons trade (captain-trade-history a-cap)))))

(defun add-to-market! (p-name t-name num)
  "Add [num] [t-good] to [p-name]s market"
  (let* ((market (planet-market (planet-name->planet p-name)))
	 (a-listing (tradegood-available? t-name market)))
    (if a-listing
	(setf (listing-amount a-listing) (+ (listing-amount a-listing) num))
	(setf market (cons (make-listing :name (string-capitalize t-name) :amount num :price (going-rate p-name t-name)) market)))))

(defun add-to-cargo! (a-cap t-name num)
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
		   (cons (make-listing :name (string-capitalize t-name) :amount num) (ship-cargo (captain-ship a-cap)))))))) ;; otherwise add a new entry

;;;;;;;;;; Updates/Deletes
(defun move-captain! (a-cap p)
  (let* ((fuel (ship-fuel (captain-ship a-cap)))
	 (current-planet (planet-name->planet (captain-current-planet a-cap)))
	 (distance (planet-distance current-planet p)))
    (setf (captain-current-planet a-cap) (planet-name p)
	  (ship-fuel (captain-ship a-cap)) (round (- fuel (* distance (ship-fuel-consumption (captain-ship a-cap))))))))

(defun process-purchase! (a-cap a-listing num)
  (let ((t-name (listing-name a-listing)))
    (setf (listing-amount a-listing) (- (listing-amount a-listing) num) ;; Remoe [num] [t-name] from the planet
	  (captain-credits a-cap) (- (captain-credits a-cap) (* num (listing-price a-listing)))) ;; Remove (* [num] [price]) credits from captains' account
    (add-to-cargo! a-cap t-name num)
    (record-trade-history! a-cap 'buy (captain-current-planet a-cap) num (string-capitalize t-name) (listing-price a-listing))
    (format nil "Bought ~a ~a" num t-name)))

(defun process-sale! (a-cap a-listing sell-price num)
  (let ((t-name (listing-name a-listing)))
    (remove-from-cargo! a-cap t-name num)
    (add-to-market! (captain-current-planet a-cap) t-name num)
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

(defun market-produce! (productivity a-market)
  (mapcar (lambda (l)
	    (let ((tech-level (max 1 (tradegood-tech-level (tradegood-name->tradegood (listing-name l))))))
	      (setf (listing-amount l) (+ (listing-amount l) (round (/ productivity tech-level))))))
	  a-market))

(defun galaxy-produce! ()
  (mapcar (lambda (a-planet)
	    (market-produce! (planet-productivity a-planet) (planet-market a-planet)))
	  *galaxy*))
	      
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
  (and (tradegood-p g)
       (eq (tradegood-type g) 'fuel)))

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
	  ((and (not a-listing) good) (generate-price (planet-radius plt) (planet-tech-level plt) (tradegood-tech-level good) (tradegood-base-price good) (roll-dice 2 4) (roll-dice 2 4))) 
	  (t nil)))) ;; If we've gotten here, it means that the tradegood given doesn't exist in game  

(defun systems-in-range (a-range p)
  "Returns a list of planets within [a-range] of planet [p]"
  (remove-if (lambda (other-planet) (equalp p other-planet))
	     (remove-if-not (lambda (other-planet)
			      (> a-range (planet-distance p other-planet)))
			    *galaxy*)))

(defun planet-distance (p1 p2)
  "Given two planets, returns the distance between them"
  (flet ((diff-sq (n1 n2) (* (- n1 n2) (- n1 n2))))
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
  (- (ship-cargo-cap s)
     (apply '+ (mapcar (lambda (a-listing) (or (listing-amount a-listing) 0)) (ship-cargo s)))))

(defun ship-fuel-space (s)
  "Returns amount of free fuel space in the given ship"
  (- (ship-fuel-cap s) (ship-fuel s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Generators
(defun generate-planet ()
  (let* ((rad (roll-dice 6 10))
	 (tech (roll-dice 2 4 (random 4)))
	 (prod (round (roll-dice 4 6 (/ rad 2)))))
    (make-planet :name (string-capitalize (grammar->string *planet-name-grammar*))
		 :description (grammar->string *planet-desc-grammar*)
		 :radius rad
		 :x (random 500) :y (random 500) :z (random 500)
		 :market (generate-market rad tech prod)
		 :tech-level tech
		 :productivity prod)))

(defun generate-price (radius tech-level good-tech-level base-price substitutes complements)
  (let ((supply (/ (+ radius tech-level) (max 1 good-tech-level)))
	(demand (+ (roll-dice radius 10) (- good-tech-level substitutes complements))))
    (round (+ base-price (/ demand (max 1 supply))))))

(defun generate-market (rad tech prod)
  (let ((possible-goods (remove-if-not (lambda (g) (>= tech (tradegood-tech-level g))) *tradegoods*)))
    (mapcar (lambda (g)
	      (let* ((amt (max 0 (/ (* prod tech) (+ 1 (tradegood-tech-level g)))))
		     (pri (generate-price rad tech (tradegood-tech-level g) (tradegood-base-price g) (roll-dice 2 4) (roll-dice 2 4))))
		(make-listing :name (tradegood-name g) 
			      :amount (round amt) :price (round pri))))
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