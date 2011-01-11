;;; Commands ;;;;;;;;;;
market ;; displays local market
local-systems ;; display a list of systems in range for the ship

buy [num] [good] ;; buy [num] [good]s from the local market
sell [num] [good] ;; sell [num] [good]s to the local market
;; fuel is a good that fills out your fuel-cap before your cargo hold
;; equipment is a good that doesn't take up cargo space, and has some sort of effect on your ship instead

dock ;;dock at the local planet
undock ;;undock from the current planet
go [planet-in-range] ;;travel to [planet-in-range]

refuel
;;;;;;;;;;;;;;;;;;;;;;

(defstruct planet
  (name nil :read-only t)
  (description nil :read-only t)
  (radius nil :read-only t)
  x y z
  market ;; (list (:tradegood [tradegood] :price [price] :quantity [quantity]) ...)
  government ;; numeric or name? either way there's a lookup (if it's a name, I need to look up the number for calculation, if it's a number, I need to look up the name each time for display purposes).
  economy
  tech-level
  population
  productivity)

(defstruct tradegood
  (base-price nil :read-only t)  ;; Base price per unit
  elasticity ;; How easily does this good respond to flooded/restricted markets?
  (type nil :read-only t) ;; right now either "goods" "fuel" "gear"
  (name nil :read-only t)
  (unit nil :read-only t))

(defstruct ship
  name
  cargo-cap
  frame
  engine
  speed
  fuel-consumption
  fuel-cap)

(defstruct captain
  name
  credits
  reputation
  xp
  current-planet
  trade-history)