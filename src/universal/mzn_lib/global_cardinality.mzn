predicate gccExact(array[int] of var int: x,
                   par bool: closed,
                   array[int] of int: cover,
                   array[int] of var int: counts);

predicate global_cardinality(array[int] of var int: x,
                                    array[int] of int: cover,
                                    array[int] of var int: counts) =
                                    gccExact(x, false, cover, counts);


