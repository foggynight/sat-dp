# sat-hs

Various SAT solving algorithms implemented in Haskell.


## DP (Davis-Putnam) or Directional Resolution

The DP algorithm uses the resolution rule to eliminate (existentially quantify)
variables until it derives an empty clause (contradiction), at which point it
can conclude the formula is unsatisfiable. If no contradiction can be derived,
it concludes the formula is satisfiable and generates a satisfying variable
assignment using the original formula and generated resolvents.

The algorithm is implemented using the "Bucket Elimination" technique described
in "The DP Algorithm" section of the "Handbook of Satisfiability" 2nd edition.


## Usage (TODO)

```sh
sat-dp [FILE]
```

- FILE: Input DIMACS CNF file, if not provided reads from stdin.
- Outputs log messages describing solving process.
- Outputs SAT/UNSAT, and list of satisfying variable assignments.
