(in-package :cl-leet)

(defun css-transform-planet (layer-size planet &key (viewport-width 600) (center-on (list 0 0)))
  (let ((ratio (/ layer-size viewport-width)))
    (flet ((px (directive) (format nil "~apx" (round (* ratio directive)))))
      (inline-css (list :left (px (+ (round (/ viewport-width 2)) (- (planet-x planet) (car center-on)))) 
			:top (px (+ (round (/ viewport-width 2)) (- (planet-y planet) (cadr center-on))))
			:height (px (* 2 (planet-radius planet))) :width (px (* 2 (planet-radius planet))))))))

(defun css-planet-class (plt current-plt local-plt-list)
  (format nil "planet p-~a~@[ ~a~]"
	  (planet-id plt)
	  (cond ((string= (planet-name current-plt) (planet-name plt)) "current")
		((member (planet-name plt) local-plt-list :test #'string=) "local")
		(t nil))))

(defun css-square (d)
  (let ((px (format nil "~apx" d)))
    (list :width px :height px)))

(defun css-box-shadow (directive) (list :-webkit-box-shadow directive :-moz-box-shadow directive :box-shadow directive))
(defvar css-control-panel '(:padding 5px :width 320px :margin-right 10px :font-size small :float left))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operations for themes
(defvar *current-theme* "/css/default-theme/")
(defmacro with-theme (a-theme &body body)
  (with-gensyms (prev-theme result)
    `(let ((,prev-theme *current-theme*))
       (setf *current-theme* ,a-theme)
       (let ((,result ,@body))
	 (setf *current-theme* ,prev-theme)
	 ,result))))

(defun theme-img (name &optional (theme *current-theme*)) (list :background-image (format nil "url(~a~a)" *current-theme* name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; compile statements
(compile-css "css/cl-leet.css"
	     `((body :font-family sans-serif)
	       (.panel :position absolute :top 590px :left 40px :z-index 9001)
	       (.top-panel ,@css-control-panel :position absolute :top 0px :left 450px :z-index 9001)
	       
	       ;; tooltip style
	       (\#tooltip :position absolute :z-index 9002 :background-color \#000 :padding 5px :max-width 250px :color \#ddd :border "1px solid #fff" :display none)
	       ("#tooltip h3" :margin 0px :padding 0px)
	       ("#tooltip p" :margin-top 0px :padding-top 0px)
	       ("#tooltip ul" :list-style-type none :padding 0px :margin "10px 0px 0px 0px")

	       (.inventory :height 150px :overflow auto)
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
	       (.viewport :width 1000px :height 600px :position absolute :overflow hidden :background-color \#000 :display block)
	       (.layer :position absolute :border "1px solid #333")
	       (.top-layer :z-index 9000 :position absolute)

	       (.planet :position absolute :background-color \#00f :border "1px solid #00c" :border-radius 80px)
	       (".planet.local" :background-color \#0f0 :border "1px solid #0c0")
	       (".planet.current" :background-color \#f00 :border "1px solid #c00")

	       (".planet:hover, .planet.local:hover, .planet.current:hover" :background-color \#666 :opacity 1)))

(defvar css-box-border (list :border-bottom "2px solid #222" :border-right "2px solid #222"))
(compile-css "css/default-theme/theme.css"
	     `((body ,@(theme-img "pattern.png"))
;;	       (.galaxy-display ,@(theme-img "galaxy-display.png") ,@css-box-border :height 670px :width 620px :position absolute :padding "20px 0px 0px 20px")
	       (.viewport ,@(theme-img "screen-reflection.png") :background-repeat no-repeat)

	       (".panel p" :padding-left 30px :padding-right 30px)

	       (".planet-info, .player-info" ,@(theme-img "side-panel.png") ,@css-box-border)

	       (.ui-button ,@(theme-img "brushed-metal-dark.png") :border-radius 15px :color "#fff" :border "none" ,@(css-box-shadow "2px 2px 0px 2px #222") :margin 3px)
	       (".ui-button.ui-button-disabled:hover" :color "#fff" ,@(css-box-shadow "2px 2px 0px 2px #222"))
	       (".ui-button:hover" :color "#f90" ,@(css-box-shadow "0px 0px 0px 1px #f90"))))