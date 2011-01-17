(require 'test-framework)

(deftest test-+
  (= (+ 1 2) 3) 
  (= (+ 1 2 3) 6) 
  (= (+ -1 -3) -4))

(provide 'leet-test)