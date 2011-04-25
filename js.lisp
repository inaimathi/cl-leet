(in-package :cl-leet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; parenscript macros/helpers
(defun compile-js (file-name js)
  (ensure-directories-exist file-name)
  (with-open-file (stream file-name :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format stream js)))

(defpsmacro $ (selector &body chains)
  `(chain (j-query ,selector) ,@chains))

(defpsmacro \ (&body body) `(lambda () ,@body))

(defpsmacro doc-ready (&body body)
  `($ document (ready (\ ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; view components
(defun planet-json (a-cap p)
  `(create :name ,(planet-name p)
	   :description ,(planet-description p)
	   :radius ,(planet-radius p)
	   :fuel ,(planet-fuel-cost a-cap p)
	   :market (list ,@(mapcar (lambda (g) 
				     `(list ,(listing-name g) 
					    ,(listing-price g) 
					    ,(tradegood-tech-level (tradegood-name->tradegood (listing-name g)))))
				   (planet-market p)))))

(defun js-planets (a-cap gal)
  `(defvar js-galaxy
     (list ,@(loop for p in gal
		collect (planet-json a-cap p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; js files
(compile-js "js/cl-leet.js"
	    (ps (defvar shift-p false)
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
								    (:span :class "label" "Fuel Cost: ") (:span :class "fuel" (@ js-galaxy (- i 1) fuel))
								    (:ul (chain (market-html (@ js-galaxy (- i 1) market)) (join "")))))))
					   (\ ($ this (css (create :opacity "0.2" :background-color "#000")))
					      ($ "#tooltip" (hide)))))))

		(defun market-html (a-market)
		  (loop for i in (chain a-market (sort (lambda (a b) (< (@ a 3) (@ b 3)))))
		     collect (who-ps-html (:li (:span :class "tradegood" (@ i 0)) ": " (:span :class "price" (@ i 1) " credits")))))

		(defun update-layer (target-layer local-x local-y)
		  ($ target-layer (css (create :left (- 0 (/ local-x (/ ($ ".galaxy-box" (width)) (- ($ target-layer (width)) ($ ".galaxy-box" (width))))))
					       :top (- 0 (/ local-y (* ($ ".galaxy-box" (height)) (- ($ target-layer (height)) ($ ".galaxy-box" (height))))))))))))