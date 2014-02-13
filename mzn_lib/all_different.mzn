predicate allDifferent(array[int] of var int: x);

predicate alldifferent(array[int] of var int: x) = allDifferent(x);

predicate all_different(array[int] of var int: x) = alldifferent(x);