Re-think how the grammars work at some point. It might be better to implement this as strings/regexps rather than lists and merging.

The semi-tail-recursion is coming back to bite you in the ass, by the by; statistical distribution of names and descriptions have some odd peaks (I'm assuming these are from the sequence clipping you do to save nesting depth)

This game will likely be much more efficient in a relational model because you want to reference by different things at different times. (Sometimes you'll want planets by x/y/z coord, sometimes by name and sometimes by tech level. Same story for goods, you want to be able to get them by tech-level, name or price. This is just in-game, btw, for other metrics, it makes even more sense to make the database external.)

Checked how the original system handles this, and it's the stupid way (one giant array, sorted by essentially planet-id, which gets traversed in full whenever the player wants to go anywhere). Granted, this doesn't have to be blazing fast, but I get the feeling that traversing a linked list of ~800 elements every move isn't a good idea.

In theory, the grammars and generated variables can be stored more efficiently in a database than as in-memory objects (this means that something like 90% of the leet-data file could be eliminated with the use of a no-sql or RDB).

;;Basic mechanics
fuel/fuel-consumption yields travel range (fuel-consumption is how much fuel the ship burns in a move of 1 units)

basically, a planet should have Econ 101 dynamics (supply/demand ratio applied as a modifier to purchases. Zero sum dynamics with complements, non-zero sum dynamics with substitutes).

;;Planet Generation
tech-level ;; Planet can only produce goods that have lower tech level than this. If a planet can't produce a given good, it is scarce. If a planet can't produce a good AND it's illegal, then holy crap profits 

demand
supply 
(maybe even just make that supply/demand ratio instead of two separate numbers)
;; Supply/demand ratio needs to be a property of the listing. Calculate based on planet tech-level and radius (the more resources, and more advanced a planet is, the better it can provide. The more substitutes a product has, the less it costs, the more complements a product has --on a market-- (meaning filter for tech level before you determine price), the more it costs)

;; ...

;; This is getting iiiiinteresting.

(let* ((supply (/ (+ 2d4 radius tech-level) (product-tech-level p)))
       (demand (+ [radius]d10 (- (product-tech-level p) (length substitues)) (length complements))))
  (* (product-base-price p)
     (/ demand supply)
     (/ complements substitutes)))

;;Goods/purchasing
gear is a good that doesn't take up cargo space, and has some sort of effect on your ship instead (this should be illustrated by an "effect" field. It should contain a function that takes a ship and returns a modified ship)

;;; Commands ;;;;;;;;;;
load-game
new-game ;; probably new/load/list/delete commander instead of game. Generate one universe at installation and call it a day

;; Compound commands
refuel ;; in terms of "buy"; buy enough fuel to fill fuel cells
;;;;;;;;;;;;;;;;;;;;;;