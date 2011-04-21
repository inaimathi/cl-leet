(defpackage :cl-leet
  (:use :cl :cl-who :cl-css :parenscript :hunchentoot) ;; :cl-ppcre :cl-fad :formlets :trivial-shell :clsql
  (:shadow :start)
  (:import-from :ironclad 
   		:encrypt-in-place :decrypt-in-place :make-cipher :octets-to-integer :integer-to-octets
   		:digest-sequence :ascii-string-to-byte-array :byte-array-to-hex-string)
  (:import-from :cl-base64 :usb8-array-to-base64-string :base64-string-to-usb8-array)
;  (:import-from :clsql-mysql :mysql-insert-id :database-mysql-ptr)
;  (:import-from :cl-smtp :send-email)
  (:import-from :sb-thread :make-thread :terminate-thread)
  (:export :start))
(in-package :cl-leet)

(defparameter *db-conn-spec* '("localhost" "clleet" "leetadmin" "cl_l33t_DB"))

;;generated with (subseq (sha256 (write-to-string (random 1024))) 0 56)
(defparameter *mdx-private-key* "d4735e3a265e16eee03f59718b9b5d03019c07d8b6c51f90da3a666e")

(defun start ()
  (defvar *web-server* (hunchentoot:start (make-instance 'acceptor :port 4141)))
  ;; (defvar *update-thread* 
  ;;   (make-thread (lambda () 
  ;; 		   (loop 
  ;; 		      do (sleep 5)
  ;; 		      do (
  ;(web-folders "js" "css" "docs")
  ;(load "js.lisp")
  )

;; ;; formlets recaptcha keys
;; (setf formlets:*public-key* "6LeSN8MSAAAAACiXL1pQr5_07ApuKzCZSKsBdQ7m"
;;       formlets:*private-key* "6LeSN8MSAAAAAE974eI_AR4t5vdPtV3RN3Csh_dJ")