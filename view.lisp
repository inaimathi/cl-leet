(in-package :cl-leet)

(defmacro page-template ((&key title) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     (:html :xmlns "http://www.w3.org/1999/xhtml" :xml\:lang "en" :lang "en"
	    (:head (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
;		   (css-links "global.css") ;;"jquery-ui.css"
;		   (js-links "jquery.js" "jquery-ui-1.8.11.custom.min.js" "jquery.tablesorter.min.js") ;;"jquery.fancybox-1.3.1.pack.js" "scrollTo.js"
		   (:title ,(format nil "~@[~A - ~]l33t" title))
		   (:body ,@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; basic interface
(define-easy-handler (captain :uri "/") ()
  (page-template (:title "Welcome")
    (:div :class "player-info" 
	  (echo-alist (cap-info))
	  (echo-cargo (cargo)))
    (:div :class "planet-info" 
	  (echo-alist (plt-info))
	  (echo-market (market)))
    (:div :class "system-info"
	  (dolist (p (local-planets))
	    (htm (:a :href (format nil "/travel?planet-name=~a" p) (str p)))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Commands (these should all be converted to links/ajax handlers in the interface)
(defun cap-info () (captain-info current-captain))
(defun plt-info () (planet-info (planet-name->planet (captain-current-planet current-captain))))
(defun market () (market-info (planet-market (planet-name->planet (captain-current-planet current-captain)))))
(defun cargo () (inventory (captain-ship current-captain)))
(defun local-planets () (list-local-planets current-captain))