# COT

Commitments of Traders https://www.cftc.gov/

Usage

```
cot --help
cot parse short.txt
cot pretty --futes --date "October 20, 2026" --contract WHEAT-SRW \
  --exchange "CHICAGO BOARD OF TRADE" \
  --under "(CONTRACTS OF 5,000 BUSHELS)" \
  --code 001602 --open-interest 506744 \
  --positions "74823 129848 87075 17174 29002 89109 121664 120140 35547 20598 35223" \
  --changes-from "October 6, 2026" \
  --changes "2265 521 -866 -328 8162 2155 5880 -2346 -1666 -3833 2347" \
  --percents "14.8 25.6 17.2 3.4 5.7 17.6 24.0 23.7 7.0 4.1 7.0" \
  --total-traders 401 --traders "67 94 25 10 16 77 53 71 63 47 60"
```

Install

```
cabal install cot
```

Development

```
cabal build
cabal haddock
cabal test
cabal run cot -- --help
```
