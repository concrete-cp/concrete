predicate int_ne_reif(var int: a, var int: b, var bool: r) =
    let { var bool: n = not(r) } in int_eq_reif(a, b, n);
   
predicate nevec(array[int] of var int: x, array[int] of var int: y);