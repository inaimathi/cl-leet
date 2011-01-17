(defmacro deftest (name &rest forms)
  `(defun ,name () (check ,name ,@forms)))

(defmacro check (test-name &rest forms)
  `(with-current-buffer (get-buffer-create "*test-results*")
     (progn (end-of-buffer)
	    (insert ,(format "====================\n== %s\n--------------------\n" test-name))
	    ,@(mapcar (lambda (f) `(report-result ',f)) forms)
	    (when (and ,@forms) (insert "   PASSED\n"))
	    (insert "\n"))
     (display-buffer "*test-results*")))

(defun report-result (form)
  (unless (eval form) (insert (format "   FAILED: %s\n     RETURNED: %s\n" form (eval (cadr form))))))

;; (deftest test-+
;;   (= (+ 1 2) 3) 
;;   (= (+ 1 2 3) 6) 
;;   (= (+ -1 -3) -4))

(provide 'test-framework)

