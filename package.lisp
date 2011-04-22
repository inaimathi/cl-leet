(defpackage :cl-leet
  (:use :cl :cl-who :cl-css :parenscript :cl-colors :hunchentoot)
  (:shadow :start)
  (:import-from :ironclad 
   		:encrypt-in-place :decrypt-in-place :make-cipher :octets-to-integer :integer-to-octets
   		:digest-sequence :ascii-string-to-byte-array :byte-array-to-hex-string)
  (:import-from :cl-base64 :usb8-array-to-base64-string :base64-string-to-usb8-array)
  (:import-from :sb-thread :make-thread :terminate-thread)
  (:export :start :reset))
(in-package :cl-leet)

(defun start ()
  (reset)
  (defvar *web-server* (hunchentoot:start (make-instance 'acceptor :port 4141)))
  (web-folders "js" "css" "img")
  ;; (defvar *update-thread* 
  ;;   (make-thread (lambda () 
  ;; 		   (loop 
  ;; 		      do (sleep 5)
  ;; 		      do (
  ;(load "js.lisp")
  )

(defun reset ()
  (defparameter *galaxy* (mapcar (lambda (n) (generate-planet)) (make-list 15)))
  (defparameter current-captain (make-captain :name "Mal"
					      :credits 10000
					      :current-planet (planet-name (car *galaxy*))
					      :trade-history nil
					      :ship (make-ship :name "Serenity"
							       :cargo-cap 10
							       :cargo nil
							       :speed 20
							       :fuel-consumption 1
							       :fuel-cap 300
							       :fuel 300))))