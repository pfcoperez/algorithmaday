# Algorithm-a-day
[![Build Status](https://travis-ci.org/pfcoperez/algorithmaday.svg?branch=master)](https://travis-ci.org/pfcoperez/algorithmaday)

This repository holds the source code of several implementations for data structures and algorithms discussed at the Tweeter feed [@DailyAlgorithm](https://twitter.com/DailyAlgorithm).

It's intended to be more a collection, a catalogue, than a library to be included in a project.

## Requests

There is a high probability of you finding flagrant absences in this catalogue, that's why it would be nice if you could ask for implementations as well as suggest changes. You can do that:

 - By opening [an issue](https://github.com/pfcoperez/algorithmaday/issues/new) at this GitHub repository. You can find an example [here](https://github.com/pfcoperez/algorithmaday/issues/7)
 - Through [algorithmaday](https://gitter.im/algorithmaday) Gitter channel.
 - Via Twitter: Direct messages or just mentions.

[@DailyAlgorithm](https://twitter.com/DailyAlgorithm) will mention you if your suggestion leads to an entry in the feed.

## Contribute

Keeping a daily feed of functional algorithm implementations is not
a task for a one-human-army. That's why contributing to this project is highly encouraged.

If these moving words haven't convinced you to loan your precious time to "the cause", you can always consider some of the following reasons:

First: Implementing algorithms in Scala, constraining yourself to functional
programming is one of the funniest puzzles you'll ever met.

Besides, from a selfish standpoint, you will:

* Get the credit in the daily tweeter feed.
* Train your problem solving skills.
* Discuss about the approaches to solve problems and learn along the way.

But, more important are some of the reasons to support the Scala & Functional Programming communities:

* Spread the word about category theory, performance, data structures and other Scala libraries.
* Demonstrate that Functional Scala can be quite a performance beast.
* Spread the knowledge on algorithmic design.

### The contributor guide to @DailyAlgorithm

Accepted contributions will:

 - Be 100% [referentially transparent](https://en.wikipedia.org/wiki/Referential_transparency).
 - Offer clean reusable code. Preferably generic.
 - Come with clear **time complexity** upper bound, at least for the average case.
 - Include a Twitter handle or other mean to refer the author in the feed.

**NOTE:** Feel free to open PRs even when those might not meet some or all of the requirements above described. The maintainer(s) will offer as much help as possible.

Find and example [here](https://github.com/pfcoperez/algorithmaday/pull/9).

Outstanding ones will:

 - Show functional patterns usage. Such as category theory application.
 - Introduce new technologies related to: Functional programming (and/or category theory) patterns, data structures, software testing and performance measurements.
 - Include one or more diagrams explaining the contribution, they will be used in the Twitter feed.

## Structure

Given the nature of its code base, this repository grows organically as
new algorithms tweets get published.

Algorithms and datastructures implementations are clustered together in packages
in function of the kind of data they manipulate or the kind of problems they
solve.

If these families grow, new package trees should be created. Take `datastructures` as an example:

* Root family [datastructures](https://github.com/pfcoperez/algorithmaday/tree/master/src/main/scala/org/pfcoperez/dailyalgorithm/datastructures):
  * [Graphs](https://github.com/pfcoperez/algorithmaday/tree/master/src/main/scala/org/pfcoperez/dailyalgorithm/datastructures/graphs): Implementation of graphs, directed or undirected, connected or not, trees or with cycles...
    * [Directed](https://github.com/pfcoperez/algorithmaday/tree/master/src/main/scala/org/pfcoperez/dailyalgorithm/datastructures/graphs/directed): Implementations of directed graphs.
      * [Trees](https://github.com/pfcoperez/algorithmaday/tree/master/src/main/scala/org/pfcoperez/dailyalgorithm/datastructures/graphs/directed/trees): Trees can be seen as a sub-type of directed graph.
    * [Undirected](https://github.com/pfcoperez/algorithmaday/tree/master/src/main/scala/org/pfcoperez/dailyalgorithm/datastructures/graphs/undirected): Sub-family of undirected graphs.
  * [Heaps](https://github.com/pfcoperez/algorithmaday/tree/master/src/main/scala/org/pfcoperez/dailyalgorithm/datastructures/heaps)
  * [Queues](https://github.com/pfcoperez/algorithmaday/blob/master/src/main/scala/org/pfcoperez/dailyalgorithm/datastructures/queues)
  * [Sets](https://github.com/pfcoperez/algorithmaday/tree/master/src/main/scala/org/pfcoperez/dailyalgorithm/datastructures/sets): As the set forest known as Union-Set or disjoint sets.
