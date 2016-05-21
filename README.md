# Queueing and Glueing Algorithm

The *queueing-glueing* algorithm is the nickname we give to an algorithmic pattern
that provides amortised linear time solutions to a number of optimal list
partition problems that have a peculiar property: at various moments we know that
two of three candidate solutions could be optimal. The algorithm works by
keeping a queue of lists, glueing them from one end, while chopping from the
other end, hence the name.

The code in this repository accompanies our paper Queueing and Glueing for Optimal Partitioning (MCL16), in which we give a formal derivation of the algorithm, and
demonstrate it with several non-trivial examples.

The code is split into the following parts:

* **OptPartList** --
  Some pseduo-code presenting the structure of the algorithm.

* **Batch, BatchTest**
  For the one-machine batching problem. To test the code, load
  `BatchTest.hs` into `ghci` and try, for example,
    `quickCheck (batch_correct 2 5 8 15)`
  where 2 is the startup overhead, 5 is the maximum weight, 8 is the maximum span, and 15
  is the maximum length of the input.

* **SizedPart, SizedPartTest**

  For the size-specific partition problem.
  Load `SizedPartTest.hs` into `ghci` and try, for example
    `quickCheck (optpart_correct 8 15)`
  where 8 is the maximum size of elements and 15 the maximum length
  of the list.

  The optimum size is `optw`, defined in `SizedPart`. The default value
  is now 25.

* **ParaFormat, ParaFormatTest**

  For paragraph formatting problem.
  Load `ParaFormatTest.hs` into `ghci` and try, for example
    `quickCheck (optpart_correct 20 15)`
  where 8 is the maximum size of words and 15 the maximum length
  of the list.

  The optimum size is `optw`, defined in ParaFormat. The default value
  is now 70.

Note: the QuickCheck properties checks the code against the
specification, which generate all the exponential number of
partitions. Therefore tests would run very slow for inputs
longer than, say, 15 elements.

## References

* (MCL16) Shin-Cheng Mu, Yu-Hsi Chiang, and Yu-Han Lyu. Queueing and glueing for optimal partitioning. In *International Conference on Functional Programming*, 2016. Accepted.
