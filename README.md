# Queueing and Glueing Algorithm

The *queueing-glueing* algorithm is the nickname we give to an algorithmic
pattern that provides amortised linear time solutions to a number of optimal
list partition problems that have a peculiar property: at various moments we
know that two of three candidate solutions could be optimal. The algorithm
works by keeping a queue of lists, glueing them from one end, while chopping
from the other end, hence the name.

The code in this repository accompanies our paper Queueing and Glueing for
Optimal Partitioning (MCL16), in which we give a formal derivation of the
algorithm, and demonstrate it with several non-trivial examples.

The code is split into the following parts:

* **OptPartList**
  A conceptual presentation of the algorithm representing queues and join lists
  using Haskell lists.

* **BatchList, BatchListTest**
  Solving the ne-machine batching problem using `OptPartList`. To test the code, load `BatchListTest.hs` into `ghci` and try, for example,
    `verboseCheck (batch_correct 2 5 8 15)`
  where 2 is the startup overhead, 5 is the maximum weight, 8 is the maximum
  span, and 15 is the maximum length of the input.

* **Batch, BatchTest**
  For the one-machine batching problem. To test the code, load
  `BatchTest.hs` into `ghci` and try, for example,
    `verboseCheck (batch_correct 2 5 8 15)`
  where 2 is the startup overhead, 5 is the maximum weight, 8 is the maximum span, and 15 is the maximum length of the input.

* **SizedPart, SizedPartTest**

  For the size-specific partition problem.
  Load `SizedPartTest.hs` into `ghci` and try, for example
    `verboseCheck (sizedpart_correct 30 8 15)`
  where 30 is the optimal size, 8 the maximum size of elements, and
  15 the maximum length of the input list.

* **ParaFormat, ParaFormatTest**

  For paragraph formatting problem.
  Load `ParaFormatTest.hs` into `ghci` and try, for example
    `verboseCheck (parafmt_correct 30 8 15)`
  where 30 the the optimal width, 8 the maximum size of words, and
  15 the maximum length of the input list.

**Note**: the QuickCheck properties checks the code against the specification,
which generate all the exponential number of partitions. Therefore tests would
run very slow for inputs longer than, say, 15 elements.

## References

* (MCL16) Shin-Cheng Mu, Yu-Hsi Chiang, and Yu-Han Lyu. Queueing and glueing for optimal partitioning. In *International Conference on Functional Programming*, 2016. Accepted.
