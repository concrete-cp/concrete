predicate count_eq(array[int] of var int: x, var int: y, var int: c); %= 
%	forall(i in dom(y)) ( y = i -> count_eq_const(x, i, c) );

%predicate count_eq_const(array[int] of var int: x, par int: y, var int: c);

predicate count_eq_reif(array[int] of var int: x, var int: y, var int: c, var bool: r) = 
  let { var 0..length(x): v } in count_eq(x, y, v) /\ int_eq_reif(v, c, r);
  
%predicate count_eq_const_reif(array[int] of var int: x, par int: y, var int: c, var bool: r) = 
%  let { var 0..length(x): v } in count_eq_const(x, y, v) /\ int_eq_reif(v, c, r);