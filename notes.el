Re-think how the grammars work at some point. It might be better to implement this as strings/regexps rather than lists and merging.

The semi-tail-recursion is coming back to bite you in the ass, by the by; statistical distribution of names and descriptions have some odd peaks (I'm assuming these are from the sequence clipping you do to save nesting depth)


This game will likely be much more efficient in a relational model because you want to reference by different things at different times. (Sometimes you'll want planets by x/y/z coord, sometimes by name and sometimes by tech level. Same story for goods, you want to be able to get them by tech-level, name or price. This is just in-game, btw, for other metrics, it makes even more sense to make the database external.)

Checked how the original system handles this, and it's the stupid way (one giant array, sorted by essentially planet-id, which gets traversed in full whenever the player wants to go anywhere). Granted, this doesn't have to be blazing fast, but I get the feeling that traversing a linked list of ~800 elements every move isn't a good idea.

In theory, the grammars and generated variables can be stored more efficiently in a database than as in-memory objects (this means that something like 90% of the leet-data file could be eliminated with the use of a no-sql or RDB).

;;Basic mechanics
fuel/fuel-consumption yields travel range (fuel-consumption is how much fuel the ship burns in a move of 1 units)

basically, a planet should have supply/demand dynamics (selling applies a penalty to price, buying gives a small bonus). Make sure the numbers are big enough that a single ship can't lock up the market.

each good should have a tech level associated with it (this will be the minimum tech level a world has to have before carrying that good). Additionally, the higher a planets tech level is, the more of an item it should generate (planets should generate items each turn, and consume some based on population. The surplus is what gets traded).

;;Planet Generation
government = (- (random 8) 1) [-1 is anarchy, the rest are named political systems]
econ [related to government. anarchy has a low econ, the higher the better (maybe should plateau off at some point), but mitigated by a random factor]
tech [related to government and econ. government is a small penalty, econ is a large bonus. Small random factor]
radius is random, but weighed towards earth radius (I should play with the units so that distance and planet size correspond roughly. Perhaps do the planet wars thing where I don't care? This requires a 2D representation though, which would be ok)
population [related to tech, econ and government. they're all bonuses, but tech and econ are much larger than gov]
productivity is [related to government, economy and population (tech should have an effect here too, but the initial engine doesn't implement one, except for the large indirect bonus through econ and pop). Government is a big bonus, so is economy, population is a multiplier]

;;Goods/purchasing
gear is a good that doesn't take up cargo space, and has some sort of effect on your ship instead (this should be illustrated by an "effect" field. It should contain a function that takes a ship and returns a modified ship)

;;; Commands ;;;;;;;;;;
load-game
new-game ;; probably new/load/list/delete commander instead of game. Generate one universe at installation and call it a day

;; Compound commands
refuel ;; in terms of "buy"
path ;; in terms of "travel"
;;;;;;;;;;;;;;;;;;;;;;