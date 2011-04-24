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

(compile-css "css/cl-leet.css"
	     `((body :font-family sans-serif)
	       (.panel :position absolute :left 620px)
	       
	       (\#tooltip :position absolute :z-index 19001 :background-color \#000 :padding 5px :width 250px :color \#ddd :border "1px solid #fff" :display none)
	       ("#tooltip h3" :margin 0px :padding 0px)
	       ("#tooltip p" :margin-top 0px :padding-top 0px)
	       ("#tooltip ul" :list-style-type none :padding 0px :margin "10px 0px 0px 0px")

	       ;; game panel
	       (.game-panel :margin-top 20px :font-weight bold)

	       ;; galaxy display
	       (.galaxy-box :width 600px :height 600px :position absolute :overflow hidden :background-color \#000 :display block)
	       (".layer, .top-layer" :border "1px solid #aaa" :position absolute)

	       (.planet :position absolute :background-color \#00f :border "1px solid #00c" :border-radius 40px)
	       (".planet.local" :background-color \#0f0 :border "1px solid #0c0")
	       (".planet.current" :background-color \#f00 :border "1px solid #c00")

	       (".planet:hover, .planet.local:hover, .planet.current:hover" :background-color \#666 :opacity 1)))