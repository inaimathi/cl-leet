(in-package :cl-leet)

(defmacro page-template ((&key title) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     (:html :xmlns "http://www.w3.org/1999/xhtml" :xml\:lang "en" :lang "en"
	    (:head (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
		   (css-links "cl-leet.css")
		   (js-links "jquery-1.5.2.min.js")
		   (:title ,(format nil "~@[~A - ~]l33t" title))
		   (:body ,@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; basic interface
(define-easy-handler (captain :uri "/") ()
  (unless (session-value :captain) (redirect "/new-game"))
  (page-template (:title "Welcome")
    (echo-galaxy-map)
    (:div :id "tooltip")
    (:div :class "panel"
	  (:div :class "player-info" 
		(echo-alist (cap-info))
		(echo-cargo (cargo)))
	  (:div :class "planet-info" 
		(echo-alist (plt-info))
		(echo-market (market))))
    (:script :type "text/javascript"
    	     (str (ps (defvar shift-p false)
    		      (doc-ready ($ ".galaxy-box" 
				    (mousemove 
				     (lambda (e)
				       (unless shift-p
					 (let* ((local-x (- (@ e page-x) ($ ".galaxy-box" (offset) left)))
						(local-y (- (@ e page-y) ($ ".galaxy-box" (offset) top))))
					   ($ ".layer" 
					      (each (\ (update-layer this local-x local-y))))

					   (loop for i from 1 to (@ ($ ".planet") length)
						do ($ (+ ".top-layer .p-" i) (offset ($ (+ ".layer .p-" i) (offset))))))))))

				 ($ document 
				    (keydown (lambda (e) (if (= (@ e which) 32) (setf shift-p t))))
				    (keyup (lambda (e) (if (= (@ e which) 32) (setf shift-p false))))
				    (mousemove (lambda (e) 
						 ($ "#tooltip" (css (create :top (+ 20 (@ e page-y)) :left (+ 20 (@ e page-x))))))))
				 
				 ($ ".planet" (each (\ ($ this (clone) (prepend-to ($ ".top-layer" (first)))))))
				 
				 (loop for i from 1 to (@ ($ ".planet") length)
				      do ($ (+ ".top-layer .p-" i) 
					    (css (create :opacity "0.2" :background-color "#000" :border-color "transparent" :z-index 9001))
					    (hover (\ ($ this (css (create :opacity "1" :background-color "#666")))
						      ($ "#tooltip" 
							 (show)
							 (html (who-ps-html (:h3 (@ js-galaxy (- i 1) name))
									    (:p (@ js-galaxy (- i 1) description))
									    (:span :class "label" "Fuel Cost: ") (:span :class "fuel" (@ js-galaxy (- i 1) fuel))))))
						   (\ ($ this (css (create :opacity "0.2" :background-color "#000")))
						      ($ "#tooltip" (hide)))))))
		      
    		      (defun update-layer (target-layer local-x local-y)
    			($ target-layer (css (create :left (- 0 (/ local-x (/ ($ ".galaxy-box" (width)) (- ($ target-layer (width)) ($ ".galaxy-box" (width))))))
    						     :top (- 0 (/ local-y (* ($ ".galaxy-box" (height)) (- ($ target-layer (height)) ($ ".galaxy-box" (height)))))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; action handlers
(define-easy-handler (new-game :uri "/new-game") ()
  (setf (session-value :captain) (generate-captain))
  (redirect "/"))

(define-easy-handler (travel :uri "/travel") (planet-name)
    (move-to-planet! (session-value :captain) (planet-name->planet (base64-string-to-string planet-name :uri t)))    
    (galaxy-produce!)
    (redirect "/"))

(define-easy-handler (buy :uri "/buy") (tradegood num) 
  (purchase! (session-value :captain) tradegood (parse-integer num))
  (redirect "/"))

(define-easy-handler (sell :uri "/sell") (tradegood num)
  (convey! (session-value :captain) tradegood (parse-integer num))
  (redirect "/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; interface components
(defun echo-alist (a-list)
  (html-to-stout
    (:ul (loop for (k v) on a-list by #'cddr
	    do (htm (:li :class (format nil "~(~a~)" k)
			  (:span :class "label" (str (format nil "~:(~a~):" k)))
			  (str v)))))))

(defun echo-cargo (cargo)
  (html-to-stout
    (:div (str (format nil "Fuel: ~a/~a" (getf cargo :fuel) (getf cargo :fuel-cap))))
    (if (not (getf cargo :cargo))
	(htm (:div "The cargo hold is empty"))
	(htm (:table (:tr (:td "Name") (:td "# in Cargo") (:td))
		     (dolist (i (getf cargo :cargo))
		       (htm (:tr (:td (str (listing-name i))) (:td (str (listing-amount i)))
				 (:td (:form :action "/sell"
					     (:input :name "tradegood" :type "hidden" :value (listing-name i))
					     (:input :name "num")
					     (:input :type "submit" :value "Sell")))))))))))	    

(defun echo-market (market)  
  (html-to-stout
    (:table (:tr (:td "Name") (:td "# Stocked") (:td "Price") (:td))
	    (dolist (i market)
	      (htm (:tr (loop for (k v) on i by #'cddr do (htm (:td (str v))))
			(:td (:form :action "/buy"
				    (:input :name "tradegood" :type "hidden" :value (getf i :name))
				    (:input :name "num")
				    (:input :type "submit" :value "Buy")))))))))

(defun planet-json (p)
  `(create :name ,(planet-name p)
	   :description ,(planet-description p)
	   :radius ,(planet-radius p)
	   :fuel ,(planet-fuel-cost *captain* p)
	   ;; :market ,(market-info (planet-market p))
	   ))

(defun js-planets (gal)
  `(defvar js-galaxy
     (list ,@(loop for p in gal
		collect (planet-json p)))))

(defun echo-galaxy-map () ;;this is actually an even split between view and model code
  (html-to-stout
    (let ((current (getf (plt-info) :name))
	  (locals (local-planets))
	  (gal (list-galaxy)))
      (htm (:div :class "galaxy-box"
		 (:script :type "text/javascript"
			  (str (ps* (js-planets *galaxy*))))
		 (dolist (d (list 100 200 300 400 500))
		   (htm (:div :class "layer" :style (inline-css `(:z-index ,d ,@(css-square d)))
			      (dolist (p (remove-if (lambda (p) (or (< (planet-z p) d) (> (planet-z p) (+ d 100)))) gal))
				(if (member (planet-name p) locals :test #'string=)
				    (htm (:a :href (format nil "/travel?planet-name=~a" (string-to-base64-string (planet-name p) :uri t)) :target "_self" :class (css-planet-class p current locals) :style (css-transform-planet d p)))
				    (htm (:div :class (css-planet-class p current locals) :style (css-transform-planet d p))))))))
		 (:div :class "top-layer" :style (inline-css `(:z-index ,600 ,@(css-square 600))))))))) ;;600 is the width of the viewport

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Commands (these should all be converted to links/ajax handlers in the interface)
(defun cap-info () (captain-info (session-value :captain)))
(defun plt-info () (planet-info (planet-name->planet (captain-current-planet (session-value :captain)))))
(defun market () (market-info (planet-market (planet-name->planet (captain-current-planet (session-value :captain))))))
(defun cargo () (inventory (captain-ship (session-value :captain))))
(defun local-planets () (list-local-planets (session-value :captain)))