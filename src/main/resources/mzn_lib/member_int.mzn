predicate member_int(array[int] of var int: x, var int: y) =
    let { var index_set(x): i } in x[i] = y;

    