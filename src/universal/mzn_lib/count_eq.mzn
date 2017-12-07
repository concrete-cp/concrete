predicate count_le(array[int] of var bool: x, par bool: y, var int: c);

predicate count_eq(array[int] of var int: x, var int: y, var int: c);

predicate count_eq_reif(array[int] of var int: x, var int: y, var int: c, var bool: r) = 
  let { var 0..length(x): v } in count_eq(x, y, v) /\ int_eq_reif(v, c, r);
