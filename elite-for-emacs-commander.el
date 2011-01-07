;;; elite-for-emacs-commander.el - Elite for EMACS commander functions

;; Player and AI commander functions. Idea is that player and AI commander have
;; exactly the same attributes
;; All game variables should be in commander struct

;;state constants are internal indicators of commander state
;;different situations may have different state, for example
;;commander can enter casino while docked and then state would be in casino etc
(defconst STATE_DOCKED 0)
(defconst STATE_IN_SPACE 1)
(defconst STATE_WITCH_SPACE 2)
(defconst STATE_COMBAT 3)

;;additional fields MUST be included at the end of struct
;;when deployed: do NOT make new fields so that they have to be available from
;;the start of game

;;todo mates;list of mates
(defstruct elite-for-emacs-commander
  id name gender
  elite-score;one kill, one point
  legal-status
  reputation;reputation, more the better
  credits
  fuel
  missiles
  current-planet
  hyperspace-system
  condition;0=docked,1=green,2=yellow,3=red
  current-state;docked,in space, witchspace, combat etc
  current-galaxy  
  cargo-capacity
  max-cargo-capacity
  current-cargo
  local-market
  lastrand
  fluct
  current-day ;day in elite unverse
  auto-refuel
  equipment-list
  script-list;script list, for good trade routes etc
  temp
  home-system
  home-galaxy
  ;;last message sent is for displaying last known system of commander
  ;;messages are public so we know it...
  distance-to-planet;;when in space, distance to planet, location 0 is space station and commander can dock,
  character-dd;;dungeons and dragons character description
  trade-history-current;;current trade items in cargo hold, update when buying
  trade-history);;overall cargo history, update when selling

(defvar elite-for-emacs-commander-list nil
  "Elite for EMACS commanders. First in list is player")

(defvar elite-for-emacs-saved-commander-list nil
  "Saved commanders");;used to easily upgrade commander struct

(defun elite-for-emacs-generate-commander (name id gender current-planet current-galaxy day)
  (setq elite-for-emacs-commander-list 
	(append 
	 elite-for-emacs-commander-list 
	 (list (make-elite-for-emacs-commander
		:id id
		:name name
		:gender gender
		:elite-score 0 ;one kill, one point
		:legal-status 0
		:condition CONDITION_DOCKED
		:current-state STATE_DOCKED
		:reputation 0 ;reputation, more the better
		:credits 1000
		:current-planet current-planet
		:current-galaxy current-galaxy
		:hyperspace-system current-planet
		:fuel 70
		:cargo-capacity 20
		:max-cargo-capacity 20
		:current-cargo (make-vector (+ lasttrade 1) 0)
		:lastrand (mysrand 12345)
		:current-day day
		:auto-refuel t ;;set nil to switch autorefuel off
		:home-system current-planet
		:home-galaxy current-galaxy)))))

(defun elite-for-emacs-get-commander (index)
  (nth index elite-for-emacs-commander-list))