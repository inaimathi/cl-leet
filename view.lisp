(in-package :cl-leet)

(defmacro page-template ((&key title) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     (:html :xmlns "http://www.w3.org/1999/xhtml" :xml\:lang "en" :lang "en"
	    (:head (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
		   (css-links "cl-leet.css")
		   (js-links "jquery-1.5.2.min.js")
		   (:title ,(format nil "~@[~A - ~]l33t" title))
		   (:body ,@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TEST
;;;;;;;;;; simple faux-3d engine (it's not real 3d, but it should still let me give the illusion to viewers).
(defun css-square (d)
  (let ((px (format nil "~apx" d)))
    (list :width px :height px)))

(define-easy-handler (nemo-orange-test :uri "/") ()
  (page-template (:title "Test")
    (:div :class "debug")
    (:div :id "enclosure"
	  (dolist (d (list 600 560 520 480 440 400))
	    (htm (:div :class "layer" :style (inline-css `(:opacity ,(float (/ d 800)) ,@(css-square d)))
		       (dotimes (i 50) (htm (:div :class "grid-square" :style (inline-css (css-square (- (/ d 5) 1))))))))))

    (:script :type "text/javascript"
	     (str (ps (defvar shift-p false)
		      (doc-ready
		       ($ "#enclosure" (mousemove 
					(lambda (e)
					  (unless shift-p
					    (let* ((local-x (- (@ e page-x) ($ "#enclosure" (offset) left)))
						   (local-y (- (@ e page-y) ($ "#enclosure" (offset) top))))
					      ($ ".layer" (each (\ (update-layer this local-x local-y)))))))))
		       ($ document 
			  (keydown (lambda (e) (if (= (@ e which) 16) (setf shift-p t))))
			  (keyup (lambda (e) (if (= (@ e which) 16) (setf shift-p false))))))
				     
		      (defun update-layer (target-layer local-x local-y)
			($ target-layer (css (create :left (- 0 (/ local-x (/ ($ "#enclosure" (width)) (- ($ target-layer (width)) ($ "#enclosure" (width))))))
						     :top (- 0 (/ local-y (* ($ "#enclosure" (height)) (- ($ target-layer (height)) ($ "#enclosure" (height)))))))))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TEST

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; basic interface
(define-easy-handler (captain :uri "/") ()
  (page-template (:title "Welcome")
    (echo-galaxy-map)
    (:div :class "player-info" 
	  (echo-alist (cap-info))
	  (echo-cargo (cargo)))
    (:div :class "planet-info" 
	  (echo-alist (plt-info))
	  (echo-market (market)))
    (:div :class "system-info"
	  (dolist (p (local-planets))
	    (htm (:a :href (format nil "/travel?planet-name=~a" p) (str p)))))

    (:script :type "text/javascript"
	     (str (ps (defvar shift-p false)
		      (doc-ready
		       ($ ".galaxy-box" (mousemove 
					(lambda (e)
					  (unless shift-p
					    (let* ((local-x (- (@ e page-x) ($ ".galaxy-box" (offset) left)))
						   (local-y (- (@ e page-y) ($ ".galaxy-box" (offset) top))))
					      ($ ".layer" (each (\ (update-layer this local-x local-y)))))))))
		       ($ document 
			  (keydown (lambda (e) (if (= (@ e which) 16) (setf shift-p t))))
			  (keyup (lambda (e) (if (= (@ e which) 16) (setf shift-p false))))))
		      
		      (defun update-layer (target-layer local-x local-y)
			($ target-layer (css (create :left (- 0 (/ local-x (/ ($ ".galaxy-box" (width)) (- ($ target-layer (width)) ($ ".galaxy-box" (width))))))
						     :top (- 0 (/ local-y (* ($ ".galaxy-box" (height)) (- ($ target-layer (height)) ($ ".galaxy-box" (height)))))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; action handlers
(define-easy-handler (travel :uri "/travel") (planet-name)
    (move-to-planet! current-captain (planet-name->planet planet-name))
    (redirect "/"))

(define-easy-handler (buy :uri "/buy") (tradegood num) 
  (purchase! current-captain tradegood (parse-integer num))
  (redirect "/"))

(define-easy-handler (sell :uri "/sell") (tradegood num)
  (convey! current-captain tradegood (parse-integer num))
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

(defun css-transform-planet (layer-size planet &optional (gal-size 500))
  (let ((ratio (/ layer-size gal-size)))
    (flet ((px (directive) (format nil "~apx" (round (* ratio directive)))))
      (inline-css (list :left (px (planet-x planet)) :top (px (planet-y planet))
			:border-radius (round (* ratio (planet-radius planet))) :height (px (* 2 (planet-radius planet))) :width (px (* 2 (planet-radius planet))))))))

(defun css-planet-class (plt current-plt local-plt-list)
  (cond ((string= current-plt plt) "planet current")
	((member plt local-plt-list) "planet local")
	(t "planet")))

(defun echo-galaxy-map ()
  (html-to-stout
    (let ((current (getf (plt-info) :name))
	  (locals (local-planets))
	  (gal (list-galaxy)))
      (htm (:div :class "galaxy-box"
		 (dolist (d (list 500 400 300 200 100))
		   (htm (:div :class "layer" :style (inline-css `(:z-index ,d ,@(css-square d)))
			      (dolist (p (remove-if (lambda (p) (or (< (planet-z p) d) (> (planet-z p) (+ d 100)))) gal))
				(htm (:div :class (css-planet-class (planet-name p) current locals) :style (css-transform-planet d p))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Commands (these should all be converted to links/ajax handlers in the interface)
(defun cap-info () (captain-info current-captain))
(defun plt-info () (planet-info (planet-name->planet (captain-current-planet current-captain))))
(defun market () (market-info (planet-market (planet-name->planet (captain-current-planet current-captain)))))
(defun cargo () (inventory (captain-ship current-captain)))
(defun local-planets () (list-local-planets current-captain))