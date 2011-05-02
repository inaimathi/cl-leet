(in-package :cl-leet)

(defun css-transform-planet (layer-size planet &key (viewport-width 600) (viewport-height viwport-width) (center-on (list 0 0)))
  (let ((ratio (/ layer-size viewport-width)))
    (flet ((px (directive) (format nil "~apx" (round (* ratio directive)))))
      (inline-css (list :left (px (+ (round (/ viewport-width 2)) (- (planet-x planet) (car center-on)))) 
			:top (px (+ (round (/ viewport-height 2)) (- (planet-y planet) (cadr center-on))))
			:height (px (* 2 (planet-radius planet))) :width (px (* 2 (planet-radius planet))))))))

(defun css-planet-class (plt current-plt local-plt-list)
  (format nil "planet p-~a~@[ ~a~]"
	  (planet-id plt)
	  (cond ((string= (planet-name current-plt) (planet-name plt)) "current")
		((member (planet-name plt) local-plt-list :test #'string=) "local")
		(t nil))))

(defun px (term) (format nil "~apx" term))
(defun css-rect (w h) (list :width (px w) :height (px h)))
(defun css-square (w) (css-rect w w))

(defun css-box-shadow (directive) (list :-webkit-box-shadow directive :-moz-box-shadow directive :box-shadow directive))
(defvar css-control-panel '(:padding 5px :width 360px :margin-right 10px :font-size small :float left))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operations for themes
(defvar *current-theme* "/css/default-theme/")
(defmacro with-theme (a-theme &body body)
  (with-gensyms (prev-theme result)
    `(let ((,prev-theme *current-theme*))
       (setf *current-theme* ,a-theme)
       (let ((,result ,@body))
	 (setf *current-theme* ,prev-theme)
	 ,result))))

(defun theme-img (name &key (theme *current-theme*) (repeat 'no-repeat)) (list :background-image (format nil "url(~a~a)" *current-theme* name) :background-repeat repeat))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; compile statements
(compile-css "css/cl-leet.css"
	     `((body :font-family sans-serif)
	       (.panel :position absolute :top 460px :left 40px :z-index 9001)
	       (.top-panel ,@css-control-panel :position absolute :top 0px :left 50% :margin-left -180px :z-index 9001)
	       
	       ;; tooltip style
	       (\#tooltip :position absolute :z-index 9005 :background-color \#000 :padding 5px :max-width 250px :color \#ddd :border "1px solid #fff" :display none)
	       ("#tooltip h3" :margin 0px :padding 0px)
	       ("#tooltip p" :margin-top 0px :padding-top 0px)
	       ("#tooltip ul" :list-style-type none :padding 0px :margin "10px 0px 0px 0px")

	       (.inventory :height 150px :overflow auto)
	       ;; (".inventory table" :width 300px)
	       ;; (".inventory table thead" :display block :background-color \#000 :color \#fff :font-weight bold :width 100%)
	       ;; (".inventory table tbody" :display block :height 150px :overflow auto :width 100%)

	       (.inventory-slider :display block :float left :width 60px :margin-top 8px)

	       ;; player info
	       (.player-info ,@css-control-panel)
	       (".player-info p" :margin-top 0px)
	       (\#refuel-tooltip :display none)

	       ;; planet info
	       (.planet-info ,@css-control-panel)
	       (.planet-name :font-weight bold)
	       (".planet-info p" :margin-top 0px)

	       ;; galaxy display
	       (.viewport :width 1000px :height 500px :position absolute :overflow hidden :background-color \#000 :display block :left 50% :margin-left -500px)
	       (.layer :position absolute :border "1px solid #333")
	       (.top-layer :z-index 9000 :position absolute)

	       (.planet :position absolute :background-color \#00f :border "1px solid #00c" :border-radius 80px)
	       (".planet.local" :background-color \#0f0 :border "1px solid #0c0")
	       (".planet.current" :background-color \#f00 :border "1px solid #c00")

	       (".planet:hover, .planet.local:hover, .planet.current:hover" :background-color \#666 :opacity 1)))

(defvar css-console-screen `(:color \#fff :font-family monospace :padding 30px))
(compile-css "css/default-theme/theme.css"
	     `((body ,@(theme-img "background.png") :background-position "top center" :margin 0px :padding 0px)
	       (.viewport ,@(theme-img "screen-reflection.png") :background-repeat no-repeat :top 10px)
	       (.top-panel ,@(theme-img "top-panel.png") :background-position "bottom center" :width 400px :margin-left -280px :padding-bottom 10px :padding-left 80px)

	       (.panel :width 1300px :left 50% :margin-left -550px)

	       (.player-info ,@(theme-img "console1.png") ,@css-console-screen :height 210px)
	       (".player-info .inventory" :height 100px)
	       
	       (.planet-info ,@(theme-img "console2.png") ,@css-console-screen :height 300px :margin-left 240px)
	       (".planet-info .inventory" :height 120px)

	       (.prop-1 ,@(theme-img "wheel.png") :position absolute :width 330px :left 50% :margin-left -180px :top 460px :height 370px :z-index 9003)

	       (.inventory :border "1px dotted #fff" :width 300px :margin 0px :padding 0px)

	       ("input.ui-button" :padding 3px :margin 5px :background \#000 :color \#fff :border "1px solid #fff")
	       ("input.ui-button:hover" :background \#fff :color \#000)))