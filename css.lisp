(in-package :cl-leet)

(defun css-transform-planet (layer-size planet &optional (gal-size 500))
  (let ((ratio (/ layer-size gal-size)))
    (flet ((px (directive) (format nil "~apx" (round (* ratio directive)))))
      (inline-css (list :left (px (planet-x planet)) :top (px (planet-y planet))
			:border-radius (round (* ratio (planet-radius planet))) :height (px (* 2 (planet-radius planet))) :width (px (* 2 (planet-radius planet))))))))

(defun css-planet-class (plt current-plt local-plt-list)
  (format nil "planet p-~a~@[ ~a~]"
	  (planet-id plt)
	  (cond ((string= current-plt (planet-name plt)) "current")
		((member (planet-name plt) local-plt-list :test #'string=) "local")
		(t nil))))

(defun css-square (d)
  (let ((px (format nil "~apx" d)))
    (list :width px :height px)))

(ensure-directories-exist "css/cl-leet.css")
(compile-css "css/cl-leet.css"
	     `((.panel :position absolute :left 620px)
	       
	       ;; galaxy display
	       (.galaxy-box :width 600px :height 600px :position absolute :overflow hidden :background-color \#000 :display block)
	       (".layer, .top-layer" :border "1px solid #aaa" :position absolute)

	       (.planet :position absolute :background-color \#00f :border "1px solid #00c" :border-radius 40px)
	       (".planet.local" :background-color \#0f0 :border "1px solid #0c0")
	       (".planet.current" :background-color \#f00 :border "1px solid #c00")

	       (".planet:hover, .planet.local:hover, .planet.current:hover" :background-color \#666 :opacity 1)))