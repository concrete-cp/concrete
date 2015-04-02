%-----------------------------------------------------------------------------%
%
% Linear equations and inequations
%
%-----------------------------------------------------------------------------%

predicate int_lin_eq_reif(array[int] of int: c, array[int] of var int: x,
                          int: d, var bool: b) = 
    let { var int: n = sum(i in index_set(x))( c[i]*x[i] ) } in 
    int_eq_reif(n, d, b);


predicate int_lin_ne_reif(array[int] of int: c, array[int] of var int: x,
                          int: d, var bool: b) =
    let { var int: n = sum(i in index_set(x))( c[i]*x[i] ) } in 
    int_ne_reif(n, d, b);

predicate int_lin_le_reif(array[int] of int: c, array[int] of var int: x,
                          int: d, var bool: b) =
    let { var int: n = sum(i in index_set(x))( c[i]*x[i] ) } in 
    int_le_reif(n, d, b);


predicate int_lin_lt_reif(array[int] of int: c, array[int] of var int: x,
                          int: d, var bool: b) =
    let { var int: n = sum(i in index_set(x))( c[i]*x[i] ) } in 
    int_lt_reif(n, d, b);
 


predicate bool_lin_le(array[int] of par int: c, array[int] of var bool: x, par int: d);

predicate array_bool_or(array[int] of var bool: v, var bool: r) = 
	bool_clause_reif(v, [], r);
