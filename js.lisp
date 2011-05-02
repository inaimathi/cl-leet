(in-package :cl-leet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; parenscript macros/helpers
(defun compile-js (file-name js)
  (ensure-directories-exist file-name)
  (with-open-file (stream file-name :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format stream js)))

(defpsmacro $ (selector &body chains) `(chain (j-query ,selector) ,@chains))

(defpsmacro \ (&body body) `(lambda () ,@body))

(defpsmacro doc-ready (&body body) `($ document (ready (\ ,@body))))

(defpsmacro math (method &rest args) `(chain -math (,method ,@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; view components
(defun js-planets (a-cap gal)
  `(defvar js-galaxy
     (list ,@(loop for p in gal
		collect (format nil (planet-tooltip-template p) (planet-fuel-cost a-cap p))))))

(defpsmacro tooltip (selector contents)
  `($ ,selector (hover (\ ($ "#tooltip" (show) (html ,contents)))
		       (\ ($ "#tooltip" (hide))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; js files
(compile-js "js/cl-leet.js"
	    (ps (doc-ready 
		 (defvar shift-p false)
		 (defvar player-credits (parse-int ($ "#player-credits" (text))))
		 (defvar player-cargo-space (- (parse-int ($ "#player-cargo-cap" (text))) (parse-int ($ "#player-cargo" (text)))))
		 (defvar galaxy-length (@ ($ ".planet" length)))

		 ;;setting up the "3D" map
		 ($ ".viewport" 
		    (mousemove 
		     (lambda (e)
		       (unless shift-p
			 (let* ((local-x (- (@ e page-x) ($ ".viewport" (offset) left)))
				(local-y (- (@ e page-y) ($ ".viewport" (offset) top))))
			   ($ ".layer" 
			      (each (\ (update-layer this local-x local-y))))
			   
			   (loop for i from 1 to galaxy-length
			      do ($ (+ ".top-layer .p-" i) (offset ($ (+ ".layer .p-" i) (offset))))))))))
		 
		 ($ document 
		    (keydown (lambda (e) (if (= (@ e which) 32) (setf shift-p t))))
		    (keyup (lambda (e) (if (= (@ e which) 32) (setf shift-p false))))
		    (mousemove (lambda (e) 
				 ($ "#tooltip" (css (create :top (+ 20 (@ e page-y)) :left (+ 20 (@ e page-x))))))))
		 
		 ($ ".planet" (each (\ ($ this (clone) (prepend-to ($ ".top-layer" (first)))))))
		 
		 (loop for i from 1 to galaxy-length
		    do (setup-planet-tooltip (+ ".top-layer .p-" i) i))

		 ;;setting up the market/inventory sliders and buttons
		 (tooltip "a.refuel-button" ($ "#refuel-tooltip" (text)))
		 ($ ".panel a, .top-panel a, input:submit, button" (css (create :font-size "small")) (button))
		 ($ "span.refuel-button" (button (create :disabled t)))

		 ($ ".player-info .inventory-slider" 
		    (each (\ (let ((max (parse-int ($ this (text)))))
			       (tooltip this ($ this (siblings "input.num-field") (val)))
			       ($ this 
				  (empty)
				  (slider (create :max max :range "min"
						  :slide (lambda (e ui) 
							   ($ this (siblings "input.num-field") (val (@ ui value)))
							   ($ "#tooltip" (html (@ ui value))))
						  :stop (\ ($ "#tooltip" (hide))))))))))
		    
		 ($ "#market-inventory .inventory-slider" 
		    (each (\ (let* ((num-left (parse-int ($ this (text))))
				    (unit-price (parse-int ($ this (parents "tr") (children ".listing-price") (text))))
				    (max (math min num-left player-cargo-space (math floor (/ player-credits unit-price)))))
			       (tooltip this ($ this (siblings "input.num-field") (val)))
			       ($ this 
				  (empty)
				  (slider (create :max max :range "min"
						  :slide (lambda (e ui) 
							   ($ this (siblings "input.num-field") (val (@ ui value)))
							   ($ "#tooltip" (html (@ ui value))))
						  :stop (\ ($ "#tooltip" (hide)))))))))))

		;; JS functions
		(defun update-slider-max (a-slider) 
		  (let* ((num-left (parse-int ($ this (parents "tr") (children ".listing-price") (text))))
			 (unit-price (parse-int ($ this (parents "tr") (children ".listing-price") (text))))
			 (max (math min num-left player-cargo-space (/ player-credits unit-price))))
		    ($ a-slider (slider "option" "max" max))))

		(defun setup-planet-tooltip (planet id)
		  ($ planet
		     (css (create :opacity "0.2" :background-color "#000" :border-color "transparent" :z-index 9001))
		     (hover (\ ($ planet (css (create :opacity "1" :background-color "#666")))
		  	       ($ "#tooltip" 
		  		  (show)
		  		  (html (@ js-galaxy (- id 1)))))
		  	    (\ ($ planet (css (create :opacity "0.2" :background-color "#000")))
		  	       ($ "#tooltip" (hide))))))

		(defun market-html (a-market)
		  (loop for i in (chain a-market (sort (lambda (a b) (< (@ a 3) (@ b 3)))))
		     collect (who-ps-html (:li (:span :class "tradegood" (@ i 0)) ": " (:span :class "price" (@ i 1) " credits")))))

		(defun update-layer (target-layer local-x local-y)
		  ($ target-layer (css (create :left (- 0 (/ local-x (/ ($ ".viewport" (width)) (- ($ target-layer (width)) ($ ".viewport" (width))))))
					       :top (- 0 (/ local-y (* ($ ".viewport" (height)) (- ($ target-layer (height)) ($ ".viewport" (height))))))))))))