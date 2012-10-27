ag44-graph-algorithms
=====================

Implementation of graph algorithms and data structures from the AG44 course. Primarily written in Haskell.

Compatibility
-------------

This package has only been tested under ubuntu linux.

Installation
------------

This package depends on the following:

*    Haskell platform: <http://www.haskell.org/platform/>

*    (optional) Graphviz for graph vizualisation: <http://www.graphviz.org/>

Once the Haskell platform is installed on your system, just run the
following in the directory containing ag44-graph-algorithms.cabal :

    cabal install

Usage
-----

  The graph_algorithms program

  graph_algorithms [COMMAND] ... [OPTIONS]

  Common flags:
    -f --file=ITEM
    -? --help       Display help message
    -V --version    Print version information

  graph_algorithms sccs [OPTIONS]

  graph_algorithms condensation [OPTIONS]

  graph_algorithms longestpath [OPTIONS]
