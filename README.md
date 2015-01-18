dependency\_graph
---

Haskell script, which crawls all python script which are in module form and draws a
dependency graph

##Getting it up

1. Get dependencies working:

```
$ sudo apt-get install graphviz graphviz-dev libgraphviz-dev
```<br>
2. Building from Cabal

```
$ cabal install
```
Assuming 

## Using it
Go to your python module, check generated_graph.png after executing this command:

```
$ dependency_graph
```

