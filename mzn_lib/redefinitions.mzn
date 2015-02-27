%-----------------------------------------------------------------------------%
% FlatZinc built-in redefinitions for linear solvers.
%
% Sebastian Brand
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
% Strict inequality
%
% Uncomment the following redefinition for FlatZinc MIP solver interfaces that
% do not support strict inequality.  Note that it does not preserve equivalence
% (some solutions of the original problem may become invalid).

% predicate float_lt(var float: x, var float: y) = x + 1e-06 <= y;

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
 

