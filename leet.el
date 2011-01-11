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

(defun generate-planet ()
  (let* ((govtype (- (random 8) 1))
	 (econ (if (> govtype 0) (logior (logand (lsh (random 3) -8) 7) 2) (logand (lsh (random 3) -8) 7)))
	 (tech (+ (logand (lsh (random 3) -8) 3) (logxor economy 7) (lsh govtype -1)))
	 (pop (+ (* 4 tech) econ govtype 1))
	 (prod (* (+ (logxor economy 7) 3) (+ 4 government) population 8)))
    (make-planet :name (capitalize (grammar->string planet-name-grammar))
		 :description (grammar->string planet-desc-grammar)
		 :radius (+ 1000 (random 7000))
		 :x (random) :y (random) :z (random)
		 :government 
		 :stats (:gov :econ :tech :pop :prod)))) ;; numeric versions of these stats, in case I need to recalculate something later

;;NOTES
;; government = (- (random 8) 1) [-1 is anarchy, the rest are named political systems]
;; econ [related to government; anarchy has a low econ, the higher the better (maybe should plateau off at some point), but mitigated by a random factor
;; tech [related to government and econ; government is a small penalty, econ is a large bonus. Small random factor]
;; radius is random, but weighed towards earth radius
;; population [related to tech, econ and government; they're all bonuses, but tech and econ are larger than gov]
;; productivity is [related to government, economy and population (tech should have an effect here too, but the initial engine doesn't implement one, except for the large indirect bonus through econ and pop). Government is a big bonus, so is economy, population is a multiplier]

;; Convenience functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pick (a-list) (nth (random (length a-list)) a-list))
(defun pick-g (key grammar) (pick (gethash key grammar))) ;;pick specialized to grammars