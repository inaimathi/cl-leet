(defmacro hash (name &rest pairs)
  `(progn
     (defvar ,name (make-hash-table))
     ,@(mapcar (lambda (pair)
		 `(puthash ',(car pair) ,(cdr pair) ,name))
	       pairs)))

(defun pick (a-list) (nth (random (length a-list)) a-list))

;;A grammar is a hash table with a key 'root whose value is a list whose elements each recursively correspond either to terminals (strings) or to further keys in the grammar
(defun pick-g (key grammar) (pick (gethash key grammar))) ;;pick specialized to grammars

(defun grammar->string (grammar)
  (expand-grammar-tc (pick-g 'root grammar) grammar))

(defun expand-grammar-tc (production grammar &optional acc)
  (cond ((not production) acc)
	((stringp production) (concat (or acc "") production))
	((symbolp production) (expand-grammar-tc (pick-g production grammar) grammar acc))
	((and (listp production) (stringp (car production)))
	 (expand-grammar-tc (cdr production) grammar (concat (or acc "") (car production))))
	(t (concat (expand-grammar-tc (car production) grammar acc)
		   (expand-grammar-tc (cdr production) grammar "")))))

;;Specific grammars
(hash planet-name-grammar
      (root . '(strict-starter
		(strict-starter strict-terminal)
		(strict-starter versatile strict-terminal)
		(strict-starter strict-link strict-terminal)
		(versatile versatile)
		(versatile strict-link versatile)))
      (strict-starter . '("at" "an" "ao" "ar" "az" (strict-starter versatile)))
      (strict-terminal . '("za" "es" "ma" "en" "be" (versatile strict-terminal)))
      (versatile . '("a" "ag" (versatile versatile)))
      (strict-link . '("on" "xe" "bi" "on" "-" "'")))

(defun generate-planet-name () 
  (capitalize (grammar->string planet-name-grammar)))