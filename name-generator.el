(defmacro hash (name &rest pairs)
  `(progn
     (defvar ,name (make-hash-table))
     ,@(mapcar (lambda (pair)
		 `(puthash ',(car pair) ,(cdr pair) ,name))
	       pairs)))

(defun pick (a-list) (nth (random (length a-list)) a-list))

;;A grammar is a hash table with a key 'root whose value is a list whose elements each recursively correspond either to terminals (strings) or to further keys in the grammar. With simple grammars (like planet-name below), a valid approach would also have been returning a list of symbols instead of a string (even then though, there would be problems with "-" and "'"). For more complex stuff (like the description generator), a lot of stuff that the engine did is easier to do with strings serving as terminals (the drawback is that you manually need to put spaces in productions of multiple non-terminals)
(defun pick-g (key grammar) (pick (gethash key grammar))) ;;pick specialized to grammars

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Specific grammars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(hash planet-name-grammar
      (root . '(strict-starter
		(strict-starter continue)
		versatile
		(versatile continue)))
      (continue . '((versatile strict-ender) 
		    (strict-link strict-ender) 
		    (strict-link versatile)
		    strict-ender 
		    versatile))
      (strict-starter . '((strict-starter versatile)
			  "at" "an" "ao" "ar" "az"))
      (strict-ender . '((versatile strict-ender)
			"za" "es" "ma" "en" "be"))
      (versatile . '((versatile versatile)
		     "a" "ag"))
      (strict-link . '((strict-link versatile) (versatile strict-link) (versatile strict-link versatile)
		       "on" "xe" "bi" "on" "-" "'")))

(defun generate-planet-name ()
  (capitalize (grammar->string planet-name-grammar)))

;;xB0 is the current planets' name
;;xB1 is the name of the current planets' inhabitants (the original grammar has it specified as "xB0ian"
;;xB2 corresponds to "random name". The original code uses the same function to generate this name as a planet name, and doesn't prevent collisions. A half-way decent way of implementing this is to add a non-terminal "random-name" to planet-desc-grammar that contains a bunch of results from (generate-planet-name)

;;As a note, the descriptions seem pretty evocative without the above. I could just remove those references and leave it at that. The other opion is to have the planet-desc-grammar return format directives instead of vanilla strings, then have the generate-description function take a planet + inhabitant name, generate a random name and generate enough directives to complete the format call. Could be interesting once everything else is refactored. A third option is to have grammar->list instead of grammar->string, change expand-grammar to accumulate using car and reverse instead of concat and have the individual generators do what's best for their situation.
(hash planet-desc-grammar
      (root . '((sentence-start planet-fact ".")))
      (sentence-start . '("\xB0" "The planet \xB0" "The world \xB0" "This planet" "This world"))
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
      (passtime . '((creature " " drink) ("\xB1 " fauna " " food) ("its " adjective " " fauna " " food) (adj-activity " " sport)
		    "cuisine" "night life" "casinos" "sit coms"))
      (historic-event . '((adj-disaster " civil war") (adj-threat " " adj-fauna " " creature "s") ("a " adj-threat " disease") 
			  (adj-disaster " earthquakes") (adj-disaster " solar activity")))
      (creature . '((fauna "oid") ("\xB1 " adj-threat)
		    fauna insect 
		    "\xB1 \xB2" "\xB2" "inhabitant"))
      
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

(defun generate-planet-description () (grammar->string planet-desc-grammar))