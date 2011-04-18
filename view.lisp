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
    (:h1 "Testing testing")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; interface components

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Commands (these should all be converted to links/ajax handlers in the interface)
(defun cap-info () (captain-info current-captain))
(defun plt-info () (planet-info (planet-name->planet (captain-current-planet current-captain))))
(defun market () (market-info (planet-market (planet-name->planet (captain-current-planet current-captain)))))
(defun cargo () (inventory (captain-ship current-captain)))
(defun local-planets () (list-local-planets current-captain))
(defun travel (p) (move-to-planet! current-captain (planet-name->planet p)))
(defun buy (t-name num) (purchase! current-captain t-name num))
(defun sell (t-name num) (convey! current-captain t-name num))