;;; elite-for-emacs-bazaar.el - Elite for EMACS bazaar

;; Commentary:

;; Bazaar is place in Elite for EMACS stations where nothing can be and happen

(defun elite-for-emacs-bazaar ()
  "Station bazaar."
  (let ()
    (insert "Entering bazaar.")
    (setf (elite-for-emacs-commander-current-state (car elite-for-emacs-commander-list)) STATE_BAZAAR)))

(defun elite-for-emacs-leave-bazaar ()
  "Leave bazaar."
  (let ()
    (insert "Leaving bazaar.")
    (setf (elite-for-emacs-commander-current-state (car elite-for-emacs-commander-list)) STATE_DOCKED)))

(defun elite-for-emacs-bazaar-prompt ()
  (let ((cmdr))
    (setq cmdr (car elite-for-emacs-commander-list))
    (concat
     (elite-for-emacs-get-system-name 
      (elite-for-emacs-commander-current-galaxy cmdr) 
      (elite-for-emacs-commander-current-planet cmdr))
     " Bazaar "
     (format "%.1f" (/ (elite-for-emacs-commander-credits cmdr) 10.0))
     ">")))