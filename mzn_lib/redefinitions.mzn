predicate bool_lin_le(array[int] of par int: c, array[int] of var bool: x, par int: d);

predicate array_bool_or(array[int] of var bool: v, var bool: r) = 
	bool_clause_reif(v, [], r);

predicate int_ne_reif(var int: a, var int: b, var bool: r) =
    let { var bool: n = not(r) } in int_eq_reif(a, b, n);