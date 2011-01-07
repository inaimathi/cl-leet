(defmacro hash (name &rest pairs)
  `(progn
     (defvar ,name (make-hash-table))
     ,@(mapcar (lambda (pair)
		 `(puthash ',(car pair) ,(cdr pair) ,name))
	       pairs)))

(defun pick (a-list) (nth (random (length a-list)) a-list))
(defun pick-g (key grammar) (pick (gethash key grammar)))

(defun tree-to-string (a-tree &optional acc)
  (cond ((not a-tree) acc)
	((stringp (car a-tree)) (tree-to-string (cdr a-tree) (concat (or acc "") (car a-tree))))
	((listp (car a-tree)) (concat (tree-to-string (car a-tree) acc)
				     (tree-to-string (cdr a-tree) "")))))

(defun flatten (list) (mapcan (lambda (x) (if (listp x) x nil)) list))


(hash planet-name-grammar
      (root . '(strict-starter
		(strict-starter strict-terminal)
		(strict-starter versatile strict-terminal)
		(strict-starter strict-link strict-terminal)
		versatile
		(versatile versatile)
		(versatile strict-link versatile)))
      (strict-starter . '("at" "an" "ao" "ar" "az" (strict-starter versatile)))
      (strict-terminal . '("za" "es" "ma" "en" "be" (versatile strict-starter)))
      (versatile . '("a" "ag" (versatile versatile)))
      (strict-link . '("on" "xe" "bi" "on")))

(defun generate-name (grammar)
  (let ((raw-name (expand-grammar (pick-g 'root grammar) grammar))) 
    (capitalize (cond ((stringp raw-name) raw-name)
		      ((listp raw-name) (tree-to-string raw-name))))))

(defun expand-grammar (production grammar)
  (cond ((symbolp production) (expand-grammar (pick-g production grammar) grammar))
	((listp production) (mapcar (lambda (p) (expand-grammar p grammar)) production))
	;((stringp production) production)
	(t production)))


(defun expand-grammar-tc (production grammar &optional acc)
  (cond ((not production) acc)
	((stringp production) (concat (or acc "") production))
	((symbolp production) (expand-grammar-tc (pick-g production grammar) grammar acc))
	((and (listp production) (stringp (car production)))
	 (expand-grammar-tc (cdr production) grammar (concat (or acc "") (car production))))
	(t (concat (expand-grammar-tc (car production) grammar acc)
		   (expand-grammar-tc (cdr production) grammar "")))))

