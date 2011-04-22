(in-package :cl-leet)

(compile-css "css/cl-leet.css"
	     `((.galaxy-box :width 600px :height 600px :overflow hidden :background-color \#000 :float left)
	       (.layer :border "1px solid #aaa" :position absolute)
	       (.grid-square :border "1px solid #fff" :border-right none :border-bottom none :float left :opacity .5)

	       (.planet :position absolute :background-color \#00f :border "1px solid #00c" :border-radius 40px)
	       (".planet:hover, .planet.local:hover, .planet.current:hover" :background-color \#666)
	       (".planet.local" :background-color \#0f0 :border "1px solid #0c0")
	       (".planet.current" :background-color \#f00 :border "1px solid #c00")))