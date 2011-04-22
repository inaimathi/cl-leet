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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; parenscript macros
(defun compile-js (file-name js)
  (with-open-file (stream file-name :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format stream js)))

;; (defun ps-highlight (&optional (color "#0f0")) `(effect "highlight" (create :color ,color) 500))

(defpsmacro $ (selector &body chains)
  `(chain (j-query ,selector)
	  ,@chains))

(defpsmacro doc-ready (&body body)
  `($ document
      (ready (\ ,@body))))

(defpsmacro \ (&body body) `(lambda () ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;document page shortcuts
(defun link-group (label/uri-list &key current-link css-class)
  (flet ((label->class (l) (format nil "toolbar-button ~a btn-~a" (or css-class "") (regex-replace-all " " (string-downcase l) "-"))))
    (html-to-stout
      (:span :class "link-group"
	     (loop for (label uri) on label/uri-list by #'cddr
		do (if (string= current-link label)
		       (htm (str label))
		       (htm (:a :href uri :class (label->class label)  (str label)))))))))

(defmacro link-uri (page-name &rest params)
  (let ((template (format nil "/~a?~{~(~a~)=~~a~^&~}" page-name params)))
    `(format nil ,template ,@params)))

(defmacro zoom-link-uri (zoom-level)
  `(let ((zoom ,zoom-level)) 
     (link-uri "view-document" doc-id zoom version-id)))

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
(defun to-str (something) (format nil "~a" something)) ;; `write-to-string` returns "\"6\"" given "6", and I really need a fixed point function for string representations

(defun blank? (str) (and (stringp str) (string= str "")))

(defun roll-dice (num-dice die-type &optional mod) ;;The simplest non-uniform dice roller I could think up without resorting to the grab-bag
  (let* ((rolls (mapcar (lambda (die) (+ 1 (random die)))
			(make-list num-dice :initial-element die-type))))
    (apply #'+ (cons (or mod 0) rolls))))