/** @group globals
  Requires that in each subsequence \a vs[\p i], ..., \a vs[\p i + \a seq - 1] the sum of the
  values belongs to the interval [\a low, \a up].
*/
predicate sliding_sum(int: low, int: up, int: seq, array[int] of var int: vs) = slidingSum(low, up, seq, vs);

predicate slidingSum(int: low, int: up, int: seq, array[int] of var int: vs);