A command-line flashcard program.

## The algorithm

Basically a digital version of the [Leitner System](http://en.wikipedia.org/wiki/Leitner_system).

There are buckets for cards you view every 0, 1, 2, 4, 8 16, 32, ... days.  A
card is presented when it comes due.  If the user gets it right it goes into
the next bigger bucket, otherwise it goes back to the beginning.  The user has
to get a card right several times in a row before it moves off the starting line.

When the program starts the user is presented with cards that are due today, as
well as a small number of new/forgotten cards.  When the number of new cards
drops below some limit, an addtional small batch of cards from a random
category is injected into the mix.

## To compile

Requires [leiningen](https://github.com/technomancy/leiningen)

    lein uberwar

## To run

    java -jar flash-1.0.0-SNAPSHOT-standalone.jar

## License

Copyright (C) 2011 FIXME

Distributed under the Eclipse Public License, the same as Clojure.
