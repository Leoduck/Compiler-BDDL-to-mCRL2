# Compiler-BDDL-to-mCRL2

A compiler from [BDDL](https://arxiv.org/abs/2303.16949) to [mCRL2](https://www.mcrl2.org) implemented in Ocaml to find winning strategies for 2 player boardgames on a grid

## Prerequisites

- Ocaml together with dune
- Opam packages: Menhir and OcamlLex

## Running the compiler

The compiler has a few different executables that can be run using the command

```dune exec [command]```

Where the command be one of the following:

---

```BDDL [input_domain] [input_problem] [output_file]``` 
Takes a BDDL domain and problem file and writes the mcrl2 file to output file, while checking if there is a winning strat for one of the players. 

---

```compall``` 
Compiles all the files in the benchmarks/GDDL_models/ and places them in translated/

Need to have run `rewr_all` before this command to add the breaker keyword to the models...

---

```compare``` 
Checks a few of the small game instances for known results

---
