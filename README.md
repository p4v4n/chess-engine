# chess-engine-clj

(_**WIP**_)

A command-line chess engine in clojure.

__Current Status:__

- Engine uses mini-max algorithm with alpha-beta pruning and searches at 3 ply depth in reasonable time.

## Installation

#### Software requirements :

- [Git](https://git-scm.com/)
- [Java](https://java.com/en/download/)
- [Leiningen](https://leiningen.org/)

## Usage

Use the words 'user' and 'engine' to specify white/black players.

ex : To play black against the engine

```
lein run engine user
```

#### __Move-Syntax:__

__Normal-Moves:__

Specify the starting+final loaction of the piece you intend to move

ex : `e2e4`

Similarly for piece-captures and enpassant

ex : `e5d6`

__Special-Moves:__

Kingside-Castling : `o-o`

QueenSide-Castling : `o-o-o`

Pawn-Promotion : `a7b8=q`  or  `a7a8=n`

### Bugs

Please file any issues you find on github with minimal sample code that demonstrates the problem.

## License

Copyright © 2017 p4v4n

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.