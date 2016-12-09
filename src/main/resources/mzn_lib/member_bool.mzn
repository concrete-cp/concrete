predicate member_bool(array[int] of var bool: x, var bool: y) =
    let { var index_set(x): i } in x[i] = y;

    