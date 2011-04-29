(in-package :cl-leet)

;; Basic Tradegood Data ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (This is up here instead of with the generated data because the market generator needs it)
(defvar *tradegoods*
  (list (make-tradegood :price '(4 12) :unit "ton" :type 'goods :tech-level 0 :name "Minerals")
	(make-tradegood :price '(8 20) :unit "group" :type 'goods :tech-level 0 :name "Slaves")
	(make-tradegood :price '(4 6) :unit "litre" :type 'fuel :tech-level 1 :name "Fuel")
	(make-tradegood :price '(6 4) :unit "hammock" :type 'goods :tech-level 2 :name "Food")
	(make-tradegood :price '(4 20 6) :unit "bottle" :type 'goods :tech-level 3 :name "Liquor")
	(make-tradegood :price '(3 6 24) :unit "roll" :type 'goods :tech-level 4 :name "Textiles")
	(make-tradegood :price '(10 12 12) :unit "unit" :type 'goods :tech-level 6 :name "Firearms")
	(make-tradegood :price '(14 20 20) :unit "sack" :type 'goods :tech-level 6 :name "Luxuries")
	(make-tradegood :price '(12 12 10) :unit "unit" :type 'goods :tech-level 7 :name "Machinery")
	(make-tradegood :price '(3 60 12) :unit "chip" :type 'goods :tech-level 8 :name "Computers")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Grammars
(defparameter *planet-name-grammar*
  ;be mindful of name probabilities if you try to reduce duplication here
  (list :root '((:starter :link :ender) (:starter :partition :ender) 
		(:starter :partition :link :ender) (:starter :partition :root) 
		(:starter :link :link :ender) (:starter :ender))
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

(defparameter *planet-desc-grammar*
  (list :root '(("is " :reputation " for " :subject) 
		("is " :emphasis " " :reputation " for " :subject)
		("is " :reputation " for " :subject " and " :subject) 
		("is " :emphasis " " :reputation " for " :subject " and " :subject)
		("is " :reputation " for " :subject " but " :adj-opposing-force " by " :historic-event)
		("is " :adj-opposing-force " by " :historic-event) 
		("a " :adj-negative " " :syn-planet))
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
	:fauna '("talking tree" "crab" "bat" "lobster" "shrew" "beast" "bison" "snake" "wolf" "yak" "leopard" "cat" "monkey" "goat" "fish" "snail" "slug" "asp" "moth" "grub" "ant") 
	:flora '((:fauna "-weed") "plant" "tulip" "banana" "corn" "carrot") 
	:scenery '("parking meters" "dust clouds" "ice bergs" "rock formations" "volcanoes") 
	:reputation '((:emphasis " " :reputation) "fabled" "notable" "well known" "famous" "noted") 
	:emphasis '("very" "mildly" "most" "reasonably") 
	:drink '("juice" "brandy" "water" "brew" "gargle blasters") 
	:sport '("hockey" "cricket" "karate" "polo" "tennis" "quiddich") 
	:food '("meat" "cutlet" "steak" "burgers" "soup") 
	:adjective '((:emphasis " " :adjective) 
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
	:syn-planet '("planet" "world" "place" "little planet" "dump")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Generated Data
(defparameter *galaxy* (generate-galaxy 100))
(defparameter *captain* (generate-captain))