# swisstable

stack build --ghc-options="-optc -mavx2" && stack ghci

b42586f
```
benchmarking lookup(seq)/small/SwissTable
time                 182.3 μs   (181.9 μs .. 182.8 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 181.6 μs   (181.4 μs .. 181.8 μs)
std dev              691.7 ns   (477.3 ns .. 1.163 μs)

benchmarking lookup(seq)/small/BasicHashTable
time                 208.9 μs   (208.2 μs .. 209.7 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 210.0 μs   (209.3 μs .. 212.2 μs)
std dev              3.939 μs   (1.139 μs .. 8.130 μs)
variance introduced by outliers: 12% (moderately inflated)

benchmarking lookup(rand)/small/SwissTable
time                 519.5 μs   (518.5 μs .. 521.1 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 520.9 μs   (520.2 μs .. 521.6 μs)
std dev              2.290 μs   (1.937 μs .. 2.778 μs)

benchmarking lookup(rand)/small/BasicHashTable
time                 232.8 μs   (232.2 μs .. 233.5 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 232.9 μs   (232.5 μs .. 233.4 μs)
std dev              1.519 μs   (1.019 μs .. 2.363 μs)
```
