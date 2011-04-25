(in-package :cl-leet)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;general hunchentoot macros
(defmacro web-folders (&body body)
  "Sets up folder dispatchers for the given folders"
  `(progn ,@(mapcar #'(lambda (f) 
			`(push (create-folder-dispatcher-and-handler ,(format nil "/~a/" f) ,(format nil "~a/" f)) *dispatch-table*))
		    body)))

(defmacro html-to-stout (&body body)
  "Outputs HTML to standard out."
  `(with-html-output (*standard-output* nil :indent t) ,@body))

(defmacro html-to-str (&body body)
  "Returns HTML as a string, as well as printing to standard-out"
  `(with-html-output-to-string (*standard-output*) ,@body))

(defmacro def-tag-list (name tag)
  "Shortcut for repetitive tags (such as css and js include statements)"
  `(defun ,name (&rest rest) 
     (html-to-stout (dolist (target rest) (htm ,tag)))))

(def-tag-list css-links (:link :href (format nil "/css/~a" target) :rel "stylesheet" :type "text/css" :media "screen"))
(def-tag-list js-links (:script :type "text/javascript" :src (format nil "/js/~a" target)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;basic encryption/decryption
(defun get-cipher (key) (make-cipher :blowfish :mode :ecb :key (ascii-string-to-byte-array key)))

(defun encrypt (plaintext password)
  (let ((cipher (get-cipher password))
	(msg (ascii-string-to-byte-array plaintext)))
    (encrypt-in-place cipher msg)
    (usb8-array-to-base64-string msg :uri t)))

(defun decrypt (ciphertext password)
  (let ((cipher (leet-cipherr password))
	(msg (base64-string-to-usb8-array ciphertext :uri t)))
    (decrypt-in-place cipher msg)
    (coerce (mapcar #'code-char (coerce msg 'list)) 'string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;other
(defun roll-dice (num-dice die-type &optional mod) ;;The simplest non-uniform dice roller I could think up without resorting to the grab-bag. Can you tell I'm a recovering D&D nerd yet?
  (let* ((rolls (mapcar (lambda (die) (+ 1 (random die)))
			(make-list num-dice :initial-element die-type))))
    (apply #'+ (cons (or mod 0) rolls))))

(defmacro gets (place &rest indicators)
  `(list ,@(loop for i in indicators
	      collect `(getf ,place ,i))))

(defun mean (&rest numbers) (round (/ (apply #'+ numbers) (length numbers))))