(in-package :cl-leet)
;; Structs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct planet
  name
  description
  radius
  x y z
  market ;; (list (:tradegood [tradegood] :price [price] :quantity [quantity]) ...)
  tech-level
  productivity)

(defstruct tradegood
  base-price  ;; Base price per unit
  tech-level ;; How advanced must a planet be to have them, and about how many will there be at a time? This may change over time.
  type ;; right now either 'goods 'fuel ('gear to be added later). This will be used as "substitute"
  complement-type
  name
  unit)

(defstruct listing
  name
  amount
  price)

(defstruct trade-record
  type ;;'buy or 'sell
  planet
  good
  amount
  price/unit)

(defstruct captain 
  name ship
  credits
  reputation
  xp
  current-planet
  trade-history)

(defstruct ship
  name
  frame
  engine
  speed
  fuel-consumption
  fuel-cap
  fuel
  cargo-cap
  cargo)

;; Basic Tradegood Data ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (This is up here instead of with the generated data because the market generator needs it)
(defvar *tradegoods*
  (list (make-tradegood :base-price 32 :unit "ton" :type 'goods :tech-level 0 :name "Minerals")
	(make-tradegood :base-price 140 :unit "group" :type 'goods :tech-level 0 :name "Slaves")
	(make-tradegood :base-price 30 :unit "gallon" :type 'fuel :tech-level 1 :name "Fuel")
	(make-tradegood :base-price 19 :unit "hammock" :type 'goods :tech-level 2 :name "Food")
	(make-tradegood :base-price 83 :unit "bottle" :type 'goods :tech-level 3 :name "Liquor")
	(make-tradegood :base-price 20 :unit "roll" :type 'goods :tech-level 4 :name "Textiles")
	(make-tradegood :base-price 124 :unit "unit" :type 'goods :tech-level 6 :name "Firearms")
	(make-tradegood :base-price 196 :unit "sack" :type 'goods :tech-level 6 :name "Luxuries")
	(make-tradegood :base-price 117 :unit "unit" :type 'goods :tech-level 7 :name "Machinery")
	(make-tradegood :base-price 154 :unit "chip" :type 'goods :tech-level 8 :name "Computers")))

;; Generators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun generate-planet ()
  (let* ((rad (roll-dice 6 10))
	 (tech (roll-dice 2 4 (random 4)))
	 (prod (roll-dice 4 6 (/ rad 2))))
    (make-planet :name (string-capitalize (grammar->string *planet-name-grammar*))
		 :description (grammar->string *planet-desc-grammar*)
		 :radius rad
		 :x (random 300) :y (random 300) :z (random 300)
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
			      :amount amt :price pri)))
	    possible-goods)))

;;A grammar is a plist with a key :root whose value is a list whose elements each recursively correspond either to terminals (strings) or to further keys in the grammar. With simple grammars (like planet-name below), a valid approach would also have been returning a list of symbols instead of a string (even then though, there would be problems with "-" and "'"). For more complex stuff (like the description generator), a lot of stuff that the engine did is easier to do with strings serving as terminals (the drawback is that you manually need to put spaces in productions of multiple non-terminals)
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
	
;; Grammars ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *planet-name-grammar*
  (list :root '((:starter :link :ender) ;be mindful of name probabilities if you try to reduce duplication here
		(:starter :partition :ender) 
		(:starter :partition :link :ender) 
		(:starter :partition :root) 
		(:starter :link :link :ender) 
		(:starter :ender))
	:starter '((:starter :link)
		   "aa" "ae" "al" "an" "ao" "ar" "at" "az" "be" "bi" "ce" "di" "ed" "en" "er" 
		   "es" "ge" "in" "is" "la" "le" "ma" "on" "or" "qu" "ra" "re" "ri" "so" "te" 
		   "ti" "us" "ve" "xe" "za")
	:ender '((:link :ender) 
		 "aa" "al" "at" "di" "ti" "so" "ce" "re" "za" "in" "ed" "or" "an" "ma" "ab" 
		 "ge" "aq" "en" "ri" "ve" "ag" "qu" "us" "es" "ex" "ae" "on" "bi" "xe" "le" 
		 "is" "er" "be" "la" "ar" "az" "io" "sb" "te" "ra" "ia" "nb")
	:link '((:link :link) (:link :link)
		"at" "an" "ri" "es" "ed" "bi" "ce" "us" "on" "er" "ti" "ve" "ra" "la" "le" 
		"ge" "i" "u" "xe" "in" "di" "so" "ar" "e" "s" "na" "is" "za" "re" "ma" "or" 
		"be" "en" "qu" "a" "n" "r" "te" "t")
	:partition '("-" "'" " ")))

(defvar *planet-desc-grammar*
  (list :root '(("This world" :planet-fact ".")
		("The planet" :planet-fact ".")
		("The world" :planet-fact ".")
		("This planet" :planet-fact ".")
		(:planet-fact ".")) 
	:planet-fact '((" " :reputation " for " :subject) 
		       (" " :reputation " for " :subject " and " :subject) 
		       (" " :reputation " for " :subject " but " :adj-opposing-force " by " :historic-event)
		       (" " :adj-opposing-force " by " :historic-event) 
		       (", a " :adj-negative " " :syn-planet))
	:subject '(("its " :adjective " " :place) 
		   ("its " :adjective " " :passtime) 
		   ("the " :adj-fauna " " :fauna) 
		   ("its inhabitants' " :adj-local-custom " " :inhabitant-property) 
		   :passtime) 
	:passtime '((:fauna " " :drink) (:fauna " " :food) 
		    ("its " :adjective " " :fauna " " :food) 
		    (:adj-activity " " :sport) 
		    "cuisine" "night-life" "casinos" "sit-coms") 
	:historic-event '((:adj-disaster " civil war") 
			  (:adj-threat " " :adj-fauna " " :fauna "s") 
			  ("a " :adj-threat " disease") 
			  (:adj-disaster " earthquakes") 
			  (:adj-disaster " solar activity")) 
	:place '((:fauna :flora " plantations") (:adj-forest " forests") :scenery "forests" "mountains" "oceans")
	:technology '(:passtime "food blenders" "tourists" "poetry" "discos") 
	:inhabitant-property '(("loathing of " :technology) ("love for " :technology) 
			       "shyness" "silliness" "mating traditions") 
	:fauna '("talking tree" "crab" "bat" "lobster" "shrew" "beast" "bison" "snake" "wolf" "yak" "leopard" "cat" "monkey" "goat" "fish" "snail" "slug" "wasp" "moth" "grub" "ant") 
	:flora '((:fauna "-weed") "plant" "tulip" "banana" "corn" "carrot") 
	:scenery '("parking meters" "dust clouds" "ice bergs" "rock formations" "volcanoes") 
	:reputation '((:emphasis " " :reputation) "fabled" "notable" "well known" "famous" "noted") 
	:emphasis '("very" "mildly" "most" "reasonably") 
	:drink '("juice" "brandy" "water" "brew" "gargle blasters") 
	:sport '("hockey" "cricket" "karate" "polo" "tennis" "quiddich") 
	:food '("meat" "cutlet" "steak" "burgers" "soup") 
	:adjective '((:emphasis :adjective) 
		     :adj-local-custom :adj-fauna :adj-forest :adj-disaster 
		     "great" "pink" "fabulous" "hoopy" "funny" "wierd" "strange" "peculiar") 
	:adj-fauna '(:adj-threat "mountain" "edible" "tree" "spotted" "exotic") 
	:adj-negative '((:adj-negative ", " :adj-negative) "boring" "dull" "tedious" "revolting") 
	:adj-local-custom '("ancient" "exceptional" "eccentric" "ingrained" "unusual") 
	:adj-forest '("tropical" "vast" "dense" "rain" "impenetrable" "exuberant") 
	:adj-disaster '("frequent" "occasional" "unpredictable" "dreadful" :adj-threat) 
	:adj-threat '("killer" "deadly" "evil" "lethal" "vicious") 
	:adj-activity '("ice" "mud" "zero-g" "virtual" "vacuum" "Australian, indoor-rules") 
	:adj-opposing-force '("beset" "plagued" "ravaged" "cursed" "scourged") 
	:syn-planet '("planet" "world" "place" "little planet" "dump"))))

;; Generated data ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defvar galaxy (mapcar (lambda (n) (generate-planet)) (make-list 15 0)))
;; (defvar current-captain (make-captain :name "Mal"
;; 				      :credits 10000
;; 				      :reputation 0
;; 				      :xp 0
;; 				      :current-planet (planet-name (car galaxy))
;; 				      :trade-history nil
;; 				      :ship (make-ship :name "Serenity"
;; 						       :cargo-cap 10
;; 						       :cargo nil
;; 						       :frame 'firefly
;; 						       :engine 'standard
;; 						       :speed 20
;; 						       :fuel-consumption 1
;; 						       :fuel-cap 150
;; 						       :fuel 150)))

;; (defvar test-cap2 (make-captain :name "Picard"
;; 				:credits 60000
;; 				:reputation 1337
;; 				:xp 40000
;; 				:current-planet (planet-name (car galaxy))
;; 				:trade-history '()
;; 				:ship (make-ship :name "Enterprise"
;; 						 :cargo-cap 10
;; 						 :cargo nil
;; 						 :frame 'federation-starship
;; 						 :engine 'federation-nacells
;; 						 :speed 50
;; 						 :fuel-consumption 0
;; 						 :fuel-cap 40
;; 						 :fuel 40)))