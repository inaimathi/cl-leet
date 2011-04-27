cl-leet v0.1

Based on Elite for Emacs ([project page](http://members.fortunecity.com/salkosuo/elite-for-emacs/))
but no longer shares any code.

# THIS PROJECT IS STILL IN EARLY DEVELOPMENT STAGES
##### but it has been upgraded from "kinda playable" to "mostly playable but ugly". It's still snippy about you buying more things than you can afford, or more than is available on a planet, but multiple people should now be able to play on the same server without anything exploding.

To use it 

- clone this repository
- load it into your lisp
- run `(cl-leet:start)`
- navigate to `http://localhost:4141` (and have other players navigate to `http://[your ip here]:4141`)

Click on green planets to travel to them (the red planet designates your current planet).
The tooltip shows you your destinations' name and description, how much fuel it takes to get there and a market summary.
Hold `space` to stop the map from rotating for easier clicking and hovering.
Buy and sell using the interface on the right. 
"Refuel" is just a shortcut to buy as much fuel as you need/can afford/is available.
"New Game" is for when you screw yourself over badly enough that you need to start a new captain.

Project Goals
=============

## Fun

Obviously. The point being that I'm trying to write a game that I'll actually have some fun playing, not just one for a hypothetical outside audience.

## Simplicity

Everything, especially the interface, should be as conceptually simple as possible. This doesn't mean the GUI should remain in the ugly pre-alpha phase, but that the number of actions a player can take should be fairly limited.

## Browser-based

The GUI will be browser based, the source is intended for people to develop, tweak and run their own servers. If you just want to play, I will eventually set up a server that you can simply browse to.

## Programmatic

It should be possible to play this game either by using the browser-based GUI, or by writing a script that interacts with the server through specified HTTP requests.


Thanks to ...
=============

- Ian Bell and David Braben (the original Elite team)
- Sami Salkosuo (the author of Elite for Emacs)

License Info...
===============

Elite for EMACS is based on Elite series by Ian Bell and David Braben.

Original Elite, (C) 1984 Ian Bell and David Braben.

Elite for EMACS uses code from original Elite and it is (C) 1984 Ian Bell and David Braben.

Additional Code by Sami Salkosuo (sami@roguemail.net)

cl-leet is a pure Common Lisp port of Elite for Emacs
It uses none of the original codebase (except for individual syllables 
in the planet names and descriptions)

Copyright (C) 2011 Leo Zovic aka Inaimathi

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see [http://www.gnu.org/licenses/](http://www.gnu.org/licenses/).

For the sake of convenience, this program includes a minified copy of
[jQuery 1.5.2](http://jquery.com/) and [jQueryUI 1.8.12](http://jqueryui.com/). jQuery and jQueryUI are released under a dual [GNU/MIT-style license](http://jquery.org/license). 
The uncompressed source code can be obtained from their [downloads](http://docs.jquery.com/Downloading_jQuery#Current_Release) [pages](http://jqueryui.com/download).
