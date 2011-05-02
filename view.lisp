(in-package :cl-leet)

(defmacro page-template ((&key title) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     (:html :xmlns "http://www.w3.org/1999/xhtml" :xml\:lang "en" :lang "en"
	    (:head (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
		   (css-links "cl-leet.css" "ui-lightness/jquery-ui-1.8.12.custom.css" "default-theme/theme.css")
		   (js-links "jquery-1.5.2.min.js" "jquery-ui-1.8.12.custom.min.js" "cl-leet.js")
		   (:title ,(format nil "~@[~A - ~]l33t" title))
		   (:body ,@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; basic interface
(define-easy-handler (captain :uri "/") ()
  (unless (session-value :captain) (redirect "/new-game"))
  (let* ((a-cap (session-value :captain))
	 (s (captain-ship a-cap))
	 (p (captain-current-planet a-cap)))
    (page-template (:title "Welcome")
      (echo-galaxy-map a-cap)
      (:div :id "tooltip")
      (:div :class "top-panel" (echo-refuel a-cap) (:a :href "/new-game" "New Game"))
      (:div :class "panel"
	    (:div :class "player-info" 
	    	  (:p (:span :class "label" "Credits: ") (:span :id "player-credits" (str (captain-credits a-cap))))
	    	  (:p (:span :class "label" "Fuel: ") (str (format nil "~a / ~a" (ship-fuel s) (ship-fuel-cap s))))
	    	  (:p (:span :class "label" "Cargo: ") (:span :id "player-cargo" (str (ship-cargo-total s))) "/" (:span :id "player-cargo-cap" (str (ship-cargo-cap s))))
	    	  (echo-inventory (ship-cargo (captain-ship a-cap)) :form 'sell))
	    (:div :class "planet-info" 
	    	  (htm (:p (:span :class "planet-name" (str (planet-name p))) (str (planet-description p)))
	    	       (:p (:span :class "label" "Radius: ") (str (planet-radius p)) (:span :class "label" "Tech Level: ") (str (planet-tech-level p)))
		       (:p (:span :class "label" "Local Goods: ") (str (format nil "~{~a~^, ~}" (planet-local-goods p))))
		       (when (planet-banned-goods p) (htm (:p (:span :class "label" "Banned Goods: ") (str (format nil "~{~a~^, ~}" (planet-banned-goods p)))))))
		  (:div :id "market-inventory"
			(echo-inventory (planet-market (captain-current-planet a-cap))))))
      (:div :class "prop-1"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; action handlers
(define-easy-handler (new-game :uri "/new-game") ()
  (setf (session-value :captain) (generate-captain))
  (redirect "/"))

(define-easy-handler (travel :uri "/travel") (planet-name)
  (move-to-planet! (session-value :captain) (lookup-planet (base64-string-to-string planet-name :uri t)))
  (galaxy-produce!)
  (redirect "/"))

(define-easy-handler (buy :uri "/buy") (tradegood num) 
  (purchase! (session-value :captain) tradegood (parse-integer num))
  (redirect "/"))

(define-easy-handler (sell :uri "/sell") (tradegood num)
  (convey! (session-value :captain) tradegood (parse-integer num))
  (redirect "/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; interface components
(defun echo-inventory (list-of-listings &key (empty "Your cargo hold is empty") (form 'buy))
  (html-to-stout
    (if list-of-listings
	(htm (:div :class "inventory"
		   (:table (:thead (:tr (:td "Name") (:td "# Stocked") (:td "Price") (when form (htm (:td)))))
			   (dolist (i list-of-listings)
			     (htm (:tr (:td (str (listing-name i))) (:td :class "listing-amount" (str (listing-amount i))) (:td :class "listing-price" (str (listing-price i)))
				       (when form (htm (:td (:form :action (format nil "/~(~a~)" form)
								   (:input :name "tradegood" :type "hidden" :value (listing-name i))
								   (:input :class "num-field" :name "num" :type "hidden" :value 0)
								   (:span :class "inventory-slider" (str (listing-amount i)))
								   (:input :type "submit" :value (str (string-capitalize form))))))))))
			   (:tfoot))))
	(htm (:p (str empty))))))

(defun echo-refuel (a-cap)
  (let* ((fuel-needed (ship-fuel-space (captain-ship a-cap)))
	 (local-fuel (lookup-listing "Fuel" (planet-market (captain-current-planet a-cap))))
	 (fuel-afford (floor (/ (captain-credits a-cap) (listing-price local-fuel))))
	 (fuel-available (listing-amount local-fuel))
	 (f (min fuel-needed fuel-afford fuel-available)))
    (html-to-stout
      (if (= 0 f)
	  (htm (:span :class "refuel-button" "Refuel"))
	  (htm (:a :class "refuel-button" :href (format nil "/buy?tradegood=Fuel&num=~a" f) (str "Refuel"))
	       (:span :id "refuel-tooltip" (str (format nil "~a fuel for ~a credits" f (* f (listing-price local-fuel))))))))))

(defun echo-galaxy-map (a-cap)
  (html-to-stout
    (let* ((current (captain-current-planet a-cap))
	   (view-center (list (planet-x current) (planet-y current)))
	   (locals (list-local-planets a-cap)))
      (htm (:div :class "galaxy-display"
		 (:div :class "viewport"
		       (:script :type "text/javascript" (str (ps* (js-planets a-cap *galaxy*))))
		       (loop for d from 350 to 1050 by 100
			  for layer in *partitioned-galaxy*  
			  do (htm (:div :class "layer" :style (inline-css `(:z-index ,d ,@(css-rect d (round (/ d 2)))))
					(dolist (p layer)
					  (let ((planet-class (css-planet-class p current locals))
						(planet-style (css-transform-planet d p :viewport-width 1000 :viewport-height 500 :center-on view-center)))
					    (if (member (planet-name p) locals :test #'string=)
						(htm (:a :href (format nil "/travel?planet-name=~a" (string-to-base64-string (planet-name p) :uri t))
							 :class planet-class :style planet-style))
						(htm (:div :class planet-class :style planet-style))))))))
		       (:div :class "top-layer" :style (inline-css `(,@(css-rect 1000 500))))))))))
