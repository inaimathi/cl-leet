(require 'leet-data)

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
		 :market '((widgets 30 10) (gewgaws 30 10) (whasits 50 10))
		 :government gov :economy econ :tech-level tech :population pop :productivity prod
		 :stats (list :gov gov :econ econ :tech tech :pop pop :prod prod)))) ;; numeric versions of these stats, in case I need to recalculate something later

(defvar galaxy (mapcar (lambda (n) (generate-planet)) (make-list 15 0)))
(defvar commander (make-captain :name "Mal"
				:credits 10000
				:reputation 0
				:xp 0
				:current-planet (planet-name (car galaxy))
				:trade-history '()
				:ship (make-ship :name "Serenity"
						 :cargo-cap 10
						 :cargo nil
						 :frame 'firefly
						 :engine 'standard
						 :speed 20
						 :fuel-consumption 2
						 :fuel-cap 10
						 :fuel 10)))
		

;;NOTES
;; government = (- (random 8) 1) [-1 is anarchy, the rest are named political systems]
;; econ [related to government; anarchy has a low econ, the higher the better (maybe should plateau off at some point), but mitigated by a random factor
;; tech [related to government and econ; government is a small penalty, econ is a large bonus. Small random factor]
;; radius is random, but weighed towards earth radius
;; population [related to tech, econ and government; they're all bonuses, but tech and econ are larger than gov]
;; productivity is [related to government, economy and population (tech should have an effect here too, but the initial engine doesn't implement one, except for the large indirect bonus through econ and pop). Government is a big bonus, so is economy, population is a multiplier]

(defun planet-info (p)
  (format "--==[ %s ]==--\n%s\nSize: % 15s\nPopulation: % 15s\nGovernment: %ys\nTech-level: % 15s"
	  (planet-name p) (planet-description p) (planet-radius p) (planet-population p) 
	  (planet-government p) (planet-tech-level p)))

(defun inventory (s)
  (let ((cargo (ship-cargo s))
	(fuel (ship-fuel s)))
    (format "%s\n%s" 
	    (if cargo
		(mapcar (lambda (i) (format "%s" i)) cargo)
	      (format "%s has nothing in her hold at the moment." (ship-name s)))
	    (if (> fuel 0)
		(format "Fuel Cells: %s/%s" fuel (ship-fuel-cap s))
	      (format "%s has nothing left in her fuel cells. Bust out the distress beacon, or abandon ship."  
		      (ship-name s))))))

(defun buy (num good))

(provide 'leet)