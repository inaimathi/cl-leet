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