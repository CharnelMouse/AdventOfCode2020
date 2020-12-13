Solutions for problems in Advent of Code 2020: https://adventofcode.com/2020

Current goals:
- Everything in base R, i.e. no loading of packages;
  - Exception: `as.bigz`, `divq.bigz`, and `mod.bigz` from `gmp` for modular arithmetic with integers too large for base R's `%/%` and `%%` to cope with. I must still write my own implementations of GCD and modular multiplicative inverse, instead of using `gcd.bigz` and `inv.bigz`.
- No regex.

I generally write for brevity rather than execution speed.
Some days have two scripts:

- the "short" one focuses on brevity / ease of reading;
- the "quick" one focuses on fast execution.
