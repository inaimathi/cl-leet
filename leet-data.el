;; Structs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct planet
  (name nil :read-only t)
  (description nil :read-only t)
  (radius nil :read-only t)
  x y z
  market ;; (list (:tradegood [tradegood] :price [price] :quantity [quantity]) ...)
  government
  economy
  tech-level
  population
  productivity
  stats)

(defstruct tradegood
  (base-price nil :read-only t)  ;; Base price per unit
  tech-level ;; How advanced must a planet be to have them, and about how many will there be at a time? This may change over time. Production rate should be calculated from this.
  (type nil :read-only t) ;; right now either 'goods 'fuel ('gear to be added later)
  (name nil :read-only t)
  (unit nil :read-only t))

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
(defvar tradegoods
  (list (make-tradegood :base-price 19
			:unit "hammock"
			:type 'goods
			:tech-level 2
			:name "Food")
	(make-tradegood :base-price 20
			:unit "roll"
			:type 'goods
			:tech-level 4
			:name "Textiles")
	(make-tradegood :base-price 140 
			:unit "group"
			:type 'goods
			:tech-level 0
			:name "Slaves")
	(make-tradegood :base-price 83
			:unit "bottle"
			:type 'goods
			:tech-level 3
			:name "Liquor")
	(make-tradegood :base-price 196
			:unit "sack"
			:type 'goods
			:tech-level 6
			:name "Luxuries")
	(make-tradegood :base-price 154 
			:unit "chip"
			:type 'goods
			:tech-level 8
			:name "Computers")
	(make-tradegood :base-price 117
			:unit "unit"
			:type 'goods
			:tech-level 7
			:name "Machinery")
	(make-tradegood :base-price 124
			:unit "unit"
			:type 'goods
			:tech-level 6
			:name "Firearms")
	(make-tradegood :base-price 32
			:unit "ton"
			:type 'goods
			:tech-level 0
			:name "Minerals")
	(make-tradegood :base-price 30
			:unit "gallon"
			:type 'fuel
			:tech-level 1
			:name "Fuel")))

;; Generators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
		 :market (generate-market tech)
		 :government gov :economy econ :tech-level tech :population pop :productivity prod
		 :stats (list :gov gov :econ econ :tech tech :pop pop :prod prod)))) ;; numeric versions of these stats, in case I need to recalculate something later

(defun generate-market (tech-level)
  (let ((possible-goods (filter (lambda (g) (>= tech-level (tradegood-tech-level g))) tradegoods)))
    (mapcar (lambda (g)
	      (make-listing :name (tradegood-name g) :amount 300 :price (tradegood-base-price g)))
	    possible-goods)))

;;A grammar is a hash table with a key 'root whose value is a list whose elements each recursively correspond either to terminals (strings) or to further keys in the grammar. With simple grammars (like planet-name below), a valid approach would also have been returning a list of symbols instead of a string (even then though, there would be problems with "-" and "'"). For more complex stuff (like the description generator), a lot of stuff that the engine did is easier to do with strings serving as terminals (the drawback is that you manually need to put spaces in productions of multiple non-terminals)
(defun grammar->string (grammar)
  (expand-grammar-tc (pick-g 'root grammar) grammar))

(defun expand-grammar-tc (production grammar &optional acc)
  (cond ((not production) acc)
	((stringp production) (concat (or acc "") production))
	((symbolp production) (expand-grammar-tc (pick-g production grammar) grammar acc))
	((and (listp production) (stringp (car production)))
	 (expand-grammar-tc (cdr production) grammar (concat (or acc "") (car production))))
	(t (concat (expand-grammar-tc (car production) grammar acc)
		   (expand-grammar-tc (cdr production) grammar "")))))

;; Grammars ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar planet-name-grammar 
  #s(hash-table size 65 test eql rehash-size 1.5 rehash-threshold 0.8 data 
		(root ((starter link ender) ;be mindful of name probabilities if you try to reduce duplication here
		       (starter partition ender) 
		       (starter partition link ender) 
		       (starter partition root) 
		       (starter link link ender) 
		       (starter ender)) 
		      starter ((starter link) 
			       "aa" "ae" "al" "an" "ao" "ar" "at" "az" "be" "bi" "ce" "di" "ed" "en" "er" 
			       "es" "ge" "in" "is" "la" "le" "ma" "on" "or" "qu" "ra" "re" "ri" "so" "te" 
			       "ti" "us" "ve" "xe" "za") 
		      ender ((link ender) 
			     "aa" "al" "at" "di" "ti" "so" "ce" "re" "za" "in" "ed" "or" "an" "ma" "ab" 
			     "ge" "aq" "en" "ri" "ve" "ag" "qu" "us" "es" "ex" "ae" "on" "bi" "xe" "le" 
			     "is" "er" "be" "la" "ar" "az" "io" "sb" "te" "ra" "ia" "nb") 
		      link ((link link) (link link) 
			    "at" "an" "ri" "es" "ed" "bi" "ce" "us" "on" "er" "ti" "ve" "ra" "la" "le" 
			    "ge" "i" "u" "xe" "in" "di" "so" "ar" "e" "s" "na" "is" "za" "re" "ma" "or" 
			    "be" "en" "qu" "a" "n" "r" "te" "t") 
		      partition ("-" "'"))))

(defvar planet-desc-grammar
  #s(hash-table size 65 test eql rehash-size 1.5 rehash-threshold 0.8 data 
		(root (("This world" planet-fact ".")
		       ("The planet" planet-fact ".")
		       ("The world" planet-fact ".")
		       ("This planet" planet-fact ".")
		       (planet-fact ".")) 
		      planet-fact ((" " reputation " for " subject) 
				   (" " reputation " for " subject " and " subject) 
				   (" " reputation " for " subject " but " adj-opposing-force " by " historic-event)
				   (" " adj-opposing-force " by " historic-event) 
				   (", a " adj-negative " " syn-planet))
		      subject (("its " adjective " " place) 
			       ("its " adjective " " passtime) 
			       ("the " adj-fauna " " fauna) 
			       ("its inhabitants' " adj-local-custom " " inhabitant-property) 
			       passtime) 
		      passtime ((fauna " " drink) (fauna " " food) 
				("its " adjective " " fauna " " food) 
				(adj-activity " " sport) 
				"cuisine" "night-life" "casinos" "sit-coms") 
		      historic-event ((adj-disaster " civil war") 
				      (adj-threat " " adj-fauna " " fauna "s") 
				      ("a " adj-threat " disease") 
				      (adj-disaster " earthquakes") 
				      (adj-disaster " solar activity")) 
		      place ((fauna flora " plantations") (adj-forest " forests") scenery "forests" "mountains" "oceans")
		      technology (passtime "food blenders" "tourists" "poetry" "discos") 
		      inhabitant-property (("loathing of " technology) ("love for " technology) 
					   "shyness" "silliness" "mating traditions") 
		      fauna ("talking tree" "crab" "bat" "lobster" "shrew" "beast" "bison" "snake" "wolf" "yak" "leopard" "cat" "monkey" "goat" "fish" "snail" "slug" "wasp" "moth" "grub" "ant") 
		      flora ((fauna "-weed") "plant" "tulip" "banana" "corn" "carrot") 
		      scenery ("parking meters" "dust clouds" "ice bergs" "rock formations" "volcanoes") 
		      reputation ((emphasis " " reputation) "fabled" "notable" "well known" "famous" "noted") 
		      emphasis ("very" "mildly" "most" "reasonably") 
		      drink ("juice" "brandy" "water" "brew" "gargle blasters") 
		      sport ("hockey" "cricket" "karate" "polo" "tennis" "quiddich") 
		      food ("meat" "cutlet" "steak" "burgers" "soup") 
		      adjective ((emphasis adjective) 
				 adj-local-custom adj-fauna adj-forest adj-disaster 
				 "great" "pink" "fabulous" "hoopy" "funny" "wierd" "strange" "peculiar") 
		      adj-fauna (adj-threat "mountain" "edible" "tree" "spotted" "exotic") 
		      adj-negative ((adj-negative ", " adj-negative) "boring" "dull" "tedious" "revolting") 
		      adj-local-custom ("ancient" "exceptional" "eccentric" "ingrained" "unusual") 
		      adj-forest ("tropical" "vast" "dense" "rain" "impenetrable" "exuberant") 
		      adj-disaster ("frequent" "occasional" "unpredictable" "dreadful" adj-threat) 
		      adj-threat ("killer" "deadly" "evil" "lethal" "vicious") 
		      adj-activity ("ice" "mud" "zero-g" "virtual" "vacuum" "Australian, indoor-rules") 
		      adj-opposing-force ("beset" "plagued" "ravaged" "cursed" "scourged") 
		      syn-planet ("planet" "world" "place" "little planet" "dump"))))

;; Generated data ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar galaxy (mapcar (lambda (n) (generate-planet)) (make-list 15 0)))
(defvar current-captain (make-captain :name "Mal"
				      :credits 10000
				      :reputation 0
				      :xp 0
				      :current-planet (planet-name (car galaxy))
				      :trade-history nil
				      :ship (make-ship :name "Serenity"
						       :cargo-cap 10
						       :cargo nil
						       :frame 'firefly
						       :engine 'standard
						       :speed 20
						       :fuel-consumption 1
						       :fuel-cap 150
						       :fuel 150)))

(provide 'leet-data)