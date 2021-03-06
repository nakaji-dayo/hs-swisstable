# swisstable

:warning: Currently, This library works on processors that support `AVX2` .

## links

- [Hackage](https://hackage.haskell.org/package/swisstable)
- [Instance definition of Data.HashTable.Class](https://github.com/nakaji-dayo/hs-swisstable-hashtables-class)

## Benchmark snapshot

### time

15564c4
```
benchmarking lookup(seq)/small/SwissTable
time                 218.0 μs   (217.3 μs .. 218.6 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 218.5 μs   (218.2 μs .. 218.8 μs)
std dev              1.074 μs   (903.7 ns .. 1.357 μs)

benchmarking lookup(seq)/small/BasicHashTable
time                 277.8 μs   (277.5 μs .. 278.1 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 277.6 μs   (277.0 μs .. 277.9 μs)
std dev              1.479 μs   (906.2 ns .. 2.655 μs)

benchmarking lookup(rand)/small/SwissTable
time                 386.7 μs   (384.2 μs .. 390.1 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 394.0 μs   (392.4 μs .. 395.4 μs)
std dev              5.017 μs   (4.124 μs .. 6.518 μs)

benchmarking lookup(rand)/small/BasicHashTable
time                 484.9 μs   (483.8 μs .. 485.8 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 484.7 μs   (484.2 μs .. 485.2 μs)
std dev              1.557 μs   (1.294 μs .. 1.956 μs)

benchmarking insert(seq)/sized/small/SwissTable
time                 1.271 ms   (1.265 ms .. 1.278 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.245 ms   (1.238 ms .. 1.252 ms)
std dev              24.58 μs   (20.19 μs .. 29.61 μs)

benchmarking insert(seq)/sized/small/BasicHashTable
time                 1.315 ms   (1.311 ms .. 1.319 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.321 ms   (1.319 ms .. 1.322 ms)
std dev              5.360 μs   (4.111 μs .. 6.704 μs)
```

### space

8557655
```
Case                                    Allocated  GCs
Swiss.insert                          362,035,256  314
Data.HashTable.ST.Basic.insert        192,201,784  108
Swiss.insert sized                    162,891,112  140
Data.HashTable.ST.Basic.insert sized  152,675,336   94
```
