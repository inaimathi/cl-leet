(in-package :cl-leet)

(defun css-transform-planet (layer-size planet &optional (gal-size 500))
  (let ((ratio (/ layer-size gal-size)))
    (flet ((px (directive) (format nil "~apx" (round (* ratio directive)))))
      (inline-css (list :left (px (planet-x planet)) :top (px (planet-y planet))
			:border-radius (round (* ratio (planet-radius planet))) :height (px (* 2 (planet-radius planet))) :width (px (* 2 (planet-radius planet))))))))

(defun css-planet-class (plt current-plt local-plt-list)
  (format nil "planet p-~a~@[ ~a~]"
	  (planet-id plt)
	  (cond ((string= (planet-name current-plt) (planet-name plt)) "current")
		((member (planet-name plt) local-plt-list :test #'string=) "local")
		(t nil))))

(defun css-square (d)
  (let ((px (format nil "~apx" d)))
    (list :width px :height px)))

(defvar css-side-panel '(:padding 5px :width 310px :margin-bottom 10px :font-size small))

(compile-css "css/cl-leet.css"
	     `((body :font-family sans-serif)
	       (.panel :position absolute :left 660px)
	       
	       (\#tooltip :position absolute :z-index 19001 :background-color \#000 :padding 5px :max-width 250px :color \#ddd :border "1px solid #fff" :display none)
	       ("#tooltip h3" :margin 0px :padding 0px)
	       ("#tooltip p" :margin-top 0px :padding-top 0px)
	       ("#tooltip ul" :list-style-type none :padding 0px :margin "10px 0px 0px 0px")

	       (\#refuel-tooltip :display none)

	       ;; player info
	       (.player-info ,@css-side-panel)
	       (".player-info p" :margin-top 0px)

	       ;; planet info
	       (.planet-info ,@css-side-panel)
	       (.planet-name :font-weight bold)
	       (".planet-info p" :margin-top 0px)

	       (".inventory-slider" :display block :float left :width 80px :margin-top 8px)

	       ;; galaxy display
	       (.viewport :width 600px :height 600px :position absolute :overflow hidden :background-color \#000 :display block)
	       (".layer, .top-layer" :border "1px solid #aaa" :position absolute)

	       (.planet :position absolute :background-color \#00f :border "1px solid #00c" :border-radius 40px)
	       (".planet.local" :background-color \#0f0 :border "1px solid #0c0")
	       (".planet.current" :background-color \#f00 :border "1px solid #c00")

	       (".planet:hover, .planet.local:hover, .planet.current:hover" :background-color \#666 :opacity 1)))

(compile-css "css/default-theme/theme.css"
	     `((body :background-image "url(/css/default-theme/pattern.png)")
	       (.galaxy-display :background-image "url(/css/default-theme/galaxy-display.png)" :height 665px :width 620px :position absolute :padding "35px 0px 0px 20px")
	       (.viewport :background-image "url(/css/default-theme/screen-reflection.png)")

	       (".panel p" :padding-left 30px :padding-right 30px)

	       (".planet-info, .player-info" :background-image "url(/css/default-theme/side-panel.png)")))