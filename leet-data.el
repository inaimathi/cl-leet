;; Structs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct planet
  (name nil :read-only t)
  (description nil :read-only t)
  (radius nil :read-only t)
  x y z
  market ;; (list (:tradegood [tradegood] :price [price] :quantity [quantity]) ...)
  government ;; numeric or name? either way there's a lookup (if it's a name, I need to look up the number for calculation, if it's a number, I need to look up the name each time for display purposes).
  economy
  tech-level
  population
  productivity)

(defstruct tradegood
  (base-price nil :read-only t)  ;; Base price per unit
  elasticity ;; How easily does this good respond to flooded/restricted markets?
  (type nil :read-only t) ;; right now either "goods" "fuel" "gear"
  (name nil :read-only t)
  (unit nil :read-only t))

(defstruct ship
  name
  cargo-cap
  frame
  engine
  speed
  fuel-consumption
  fuel-cap)

(defstruct captain
  name
  credits
  reputation
  xp
  current-planet
  trade-history)

;; Grammars ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(hash planet-name-grammar ;be mindful of name probabilities if you try to reduce duplication here
      (root . '((starter link ender)
		(starter partition ender)
		(starter partition link ender)
		(starter partition root)
		(starter link link ender)
		(starter ender)
		starter))
      (starter . '((starter link)
		   "aa" "ae" "al" "an" "ao" "ar" "at" "az" "be" "bi" "ce" "di" "ed" "en" "er" 
		   "es" "ge" "in" "is" "la" "le" "ma" "on" "or" "qu" "ra" "re" "ri" "so" "te" 
		   "ti" "us" "ve" "xe" "za"))
      (ender . '((link ender)
		 "aa" "al" "at" "di" "ti" "so" "ce" "re" "za" "in" "ed" "or" "an" "ma" 
		 "ab" "ge" "aq" "en" "ri" "ve" "ag" "qu" "us" "es" "ex" "ae" "on" "bi" 
		 "xe" "le" "is" "er" "be" "la" "ar" "az" "io" "sb" "te" "ra" "ia" "nb"))
      (link . '((link link) (link link)
		"at" "an" "ri" "es" "ed" "bi" "ce" "us" "on" "er" "ti" "ve" "ra" "la" 
		"le" "ge" "i" "u" "xe" "in" "di" "so" "ar" "e" "s" "na" "is" "za" "re" 
		"ma" "or" "be" "en" "qu" "a" "n" "r" "te" "t"))
      (partition . '("-" "'")))

;;xB0 is the current planets' name
;;xB1 is the name of the current planets' inhabitants (the original grammar has it specified as "xB0ian"
;;xB2 corresponds to "random name". The original code uses the same function to generate this name as a planet name, and doesn't prevent collisions. A half-way decent way of implementing this is to add a non-terminal "random-name" to planet-desc-grammar that contains a bunch of results from (generate-planet-name)

(hash planet-desc-grammar
      (root . '((sentence-start planet-fact ".")))
      (sentence-start . '("" "The planet " "The world " "This planet" "This world"))
      (planet-fact . '((" " reputation " for " subject)
		       (" " emphasis " " reputation " for " subject)
		       (" " emphasis " " reputation " for " subject follow-up-fact) 
		       (" " adj-opposing-force " by " historic-event) 
		       (", a " adj-negative " " syn-planet)))
      (follow-up-fact . '((" and " subject)
			  (" but " adj-opposing-force " by " historic-event)))
      (subject . '(("its " adjective " " place) 
		   ("its " adjective " " passtime)
		   ("the \xB1 " adj-fauna " " creature) 
		   ("its inhabitants' " adj-local-custom " " inhabitant-property) 
		   passtime))
      (passtime . '((creature " " drink) (fauna " " food) ("its " adjective " " fauna " " food) (adj-activity " " sport)
		    "cuisine" "night life" "casinos" "sit coms"))
      (historic-event . '((adj-disaster " civil war") (adj-threat " " adj-fauna " " creature "s") ("a " adj-threat " disease") 
			  (adj-disaster " earthquakes") (adj-disaster " solar activity")))
      (creature . '((fauna "oid") ("\xB2 " adj-threat)
		    fauna insect 
		    "inhabitant"))
      
      (place . '((creature flora " plantations") (adj-forest " forests") scenery 
		 "forests" "mountains" "oceans"))
      (technology . '(passtime "food blenders" "tourists" "poetry" "discos"))
      (inhabitant-property . '(("loathing of " technology) ("love for " technology)
			       "shyness" "silliness" "mating traditions"))
      
      (fauna . '("talking tree" "crab" "bat" "lobster" "shrew" "beast" "bison" "snake" "wolf" "yak" "leopard" "cat" "monkey" "goat" "fish" "snail" "slug" "\xB2"))
      (flora . '(("\xB2" "weed") "plant" "tulip" "banana" "corn" "carrot"))
      (insect . '("wasp" "moth" "grub" "ant" "\xB2"))
      
      (scenery .
 '("parking meters" "dust clouds" "ice bergs" "rock formations" "volcanoes"))
      (reputation . '("fabled" "notable" "well known" "famous" "noted"))
      (emphasis . '("very" "mildly" "most" "reasonably"))
      
      (drink . '("juice" "brandy" "water" "brew" "gargle blasters"))
      (sport . '("hockey" "cricket" "karate" "polo" "tennis" "quiddich"))
      (food . '("meat" "cutlet" "steak" "burgers" "soup"))
      
      (adjective . '((emphasis adjective) (adjective ", " adjective)
		     adj-local-custom adj-fauna adj-forest adj-disaster
		     "great" "pink" "fabulous" "hoopy" "funny" "wierd" "strange" "peculiar"))
      (adj-fauna . '(adj-threat "mountain" "edible" "tree" "spotted" "exotic"))
      (adj-negative . '((adj-negative ", " adj-negative) "boring" "dull" "tedious" "revolting"))
      (adj-local-custom . '("ancient" "exceptional" "eccentric" "ingrained" "unusual"))
      (adj-forest . '("tropical" "vast" "dense" "rain" "impenetrable" "exuberant"))
      (adj-disaster . '("frequent" "occasional" "unpredictable" "dreadful" "deadly"))
      (adj-threat . '("killer" "deadly" "evil" "lethal" "vicious"))
      (adj-activity . '("ice" "mud" "zero-g" "virtual" "vacuum" "Australian, indoor-rules"))

      (adj-opposing-force . '("beset" "plagued" "ravaged" "cursed" "scourged"))
      (syn-planet . '("planet" "world" "place" "little planet" "dump")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convenience functions ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro hash (name &rest pairs)
  `(progn
     (defvar ,name (make-hash-table))
     ,@(mapcar (lambda (pair)
		 `(puthash ',(car pair) ,(cdr pair) ,name))
	       pairs)))


;; These were used to help generate the planet-name grammar from a bunch of cool-sounding names
(defun break-string (str fragment-length)
  (let ((frag-str (number-sequence 0 (/ (length str) fragment-length))))
    (remove "" (mapcar (lambda (i)
			 (substring str (* i fragment-length) 
				    (min (length str)
					 (+ fragment-length (* i fragment-length)))))
		       frag-str))))

(defun partition-string (str)
  (let ((broken (break-string str 2)))
    (list (car broken)
	  (butlast (cdr broken))
	  (car (last broken)))))

(defun word-list->planet-grammar (list-of-words)
  "Expects a bunch of words separated by newlines or spaces. Elite for Emacs 0.1 had a bunch of planet names generated; this was the easiest way of getting a grammar that included all of them"
  (let ((words (split-string list-of-words)) ;;(with-current-buffer (buffer-string))
	(starter) (link) (ender))
    (mapcar (lambda (a-word)
	      (let ((p (partition-string a-word)))
		(add-to-list 'starter (car p))
		(mapcar (lambda (a) (add-to-list 'link a)) (cadr p))
		(add-to-list 'ender (caddr p))))
	    words)
    (list starter link ender)))