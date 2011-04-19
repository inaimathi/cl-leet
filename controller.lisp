(in-package :cl-leet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Actions
(defun move-to-planet! (a-cap p)
  "Takes a captain and a planet, and moves the captain to the planet if its within range"
  (if (within-distance? a-cap p)
      (move-captain! a-cap p)
      (error "Planet out of range")))


    (if (>= fuel-range distance)
	(move-captain! a-cap p fuel)
	(error "Planet out of range"))))

(defun purchase! (a-cap t-name num)
  "Check if a purchase order is valid, and if so, fulfill it"
  (let ((a-listing (tradegood-available? t-name (planet-market (planet-name->planet (captain-current-planet a-cap))))))
    (cond ((not a-listing) (error "That's not available at this planet"))
	  ((< (listing-amount a-listing) num) (error (format nil "They don't have that many ~a" t-name)))
	  ((< (captain-credits a-cap) (* num (listing-price a-listing))) (error (format nil "You can't afford that many ~a" t-name)))
	  ((not (enough-space? a-cap t-name num)) (error "You don't have enough room in your cargo hold"))
	  (t (process-purchase! a-cap a-listing num)))))

(defun convey! (a-cap t-name num)
  "Check if a sell order is valid, and if so, fulfill it"
  (let ((sell-price (going-rate (captain-current-planet a-cap) t-name))
	(a-listing (tradegood-available? t-name (ship-cargo (captain-ship a-cap)))))
    (cond ((not sell-price) (error (format nil "I have no clue what a ~a is" t-name)))
	  ((not a-listing) (error (format nil "You don't have any ~a in your hold" t-name)))
	  ((> num (listing-amount a-listing)) (error (format nil "You don't have enough ~a in your hold" t-name)))
	  (t (process-sale! a-cap a-listing sell-price num)))))