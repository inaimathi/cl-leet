(require 'test-framework)

(defvar test-cap (make-captain :name "Mal"
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
						:fuel-consumption 1
						:fuel-cap 150
						:fuel 120)))

(defvar test-cap2 (make-captain :name "Picard"
				:credits 60000
				:reputation 1337
				:xp 40000
				:current-planet (planet-name (car galaxy))
				:trade-history '()
				:ship (make-ship :name "Enterprise"
						 :cargo-cap 10
						 :cargo nil
						 :frame 'federation-starship
						 :engine 'federation-nacells
						 :speed 50
						 :fuel-consumption 0
						 :fuel-cap 40
						 :fuel 40)))

(deftest test-fuel-space
  (= (ship-fuel-space (captain-ship test-cap)) 30)
  (= (ship-fuel-space (captain-ship test-cap2)) 0))

(provide 'leet-test)