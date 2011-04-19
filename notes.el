This game will likely be much more efficient in a relational model because you want to reference by different things at different times. (Sometimes you'll want planets by x/y/z coord, sometimes by name and sometimes by tech level. Same story for goods, you want to be able to get them by tech-level, name or price. This is just in-game, btw, for other metrics, it makes even more sense to make the database external.)

In theory, the grammars and generated variables can be stored more efficiently in a database than as in-memory objects (this means that something like 90% of the leet-data file could be eliminated with the use of a no-sql or RDB).

;;Basic mechanics
- moving with a ship costs fuel ;;should it really? Pedaaaaantic...
- fuel-consumption yields travel range

- should be able to move between settlements on a given planet
- possibly start off as a caravan trader (food/water limit rather than fuel. When you make enough for a ship, you can get off-world)

- settlements and planets should have Econ 101 dynamics (supply/demand ratio applied as a modifier to purchases. Zero sum dynamics with complements, non-zero sum dynamics with substitutes)

- I'm thinking of using the d20 Modern SRD as the basic mechanic set for this world. The only change is how wealth works (there either needs to be a standardized way to convert between the wealth-checks and a standard currency/barter system, or else just outright switch to currency/barter).
- This eventually implies turn-based combat
- This eventually implies a turn-based MMO (do you feel like implementing what you and Mark talked about, but rather more simplified? I think so :D)

;;Goods/purchasing
- gear is a good that doesn't take up cargo space, and has some sort of effect instead (this should be illustrated by an "effect" field. It should contain a function that takes a ship and returns a modified ship. It might pay to make use of CLOS and make these methods instead)

;;;;; Basic game process
- create commander
- trade/quest/travel loop
- ("quitting" is just logging out, "loading" is just logging in)