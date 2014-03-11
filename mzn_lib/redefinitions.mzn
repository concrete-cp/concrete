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
    aux_int_eq_iff_1(sum(i in index_set(x))( c[i]*x[i] ), d, bool2int(b));


predicate int_lin_ne_reif(array[int] of int: c, array[int] of var int: x,
                          int: d, var bool: b) =
    aux_int_eq_iff_1(sum(i in index_set(x))( c[i]*x[i] ), d, 1 - bool2int(b));


predicate int_lin_le_reif(array[int] of int: c, array[int] of var int: x,
                          int: d, var bool: b) =
    let { var 0..1: p = bool2int(b) }
    in
    aux_int_le_if_1(sum(i in index_set(x))( c[i] * x[i] ), d, p) /\
    aux_int_gt_if_0(sum(i in index_set(x))( c[i] * x[i] ), d, p);


predicate int_lin_lt_reif(array[int] of int: c, array[int] of var int: x,
                          int: d, var bool: b) =
    int_lin_le_reif(c, x, d - 1, b);


%-----------------------------------------------------------------------------%
% Auxiliary: equality reified onto a 0/1 variable

predicate aux_int_eq_iff_1(var int: x, var int: y, var int: p) =
    let { array[1..2] of var 0..1: q }
    in
    aux_int_lt_if_0(x - p, y, q[1]) /\
    aux_int_gt_if_0(x + p, y, q[2]) /\
    sum(q) <= 2 - 2*p /\
    sum(q) <= 1 + p;

    % Alternative 1
predicate alt_1_aux_int_eq_iff_1(var int: x, var int: y, var int: p) =
    let { array[1..2] of var 0..1: q }
    in
    aux_int_lt_if_0(x - p, y, q[1]) /\
    aux_int_gt_if_0(x + p, y, q[2]) /\
    q[1] <= 1 - p /\
    q[2] <= 1 - p /\
    sum(q) <= 1 + p;

    % Alternative 2
predicate alt_2_aux_int_eq_iff_1(var int: x, var int: y, var int: p) =
    let { array[1..2] of var 0..1: q }
    in
    aux_int_le_if_1(x, y, p) /\
    aux_int_ge_if_1(x, y, p) /\
    aux_int_lt_if_0(x, y, q[1]) /\
    aux_int_gt_if_0(x, y, q[2]) /\
    sum(q) <= p + 1;


%predicate aux_float_eq_iff_1(var float: x, var float: y, var float: p) =
%    let { array[1..2] of var 0..1: q }
%    in
%    aux_float_le_if_1(x, y, p) /\
%    aux_float_ge_if_1(x, y, p) /\
%    aux_float_lt_if_0(x, y, int2float(q[1])) /\
%    aux_float_gt_if_0(x, y, int2float(q[2])) /\
%    int2float(sum(q)) <= 1.0 + p;

%-----------------------------------------------------------------------------%
% Auxiliary: indicator constraints
%   p -> x # 0  where p is a 0/1 variable and # is a comparison

% Base cases

predicate aux_int_le_zero_if_0(var int: x, var int: p) =
    x <= ub(x) * p;

predicate aux_float_le_zero_if_0(var float: x, var float: p) =
    x <= ub(x) * p;

%predicate aux_float_lt_zero_if_0(var float: x, var float: p) =
%    let { float: rho = 1e-02 * abs(ub(x)) }  % same order of magnitude as ub(x)
%    in
%    x < (ub(x) + rho) * p;


% Derived cases

predicate aux_int_le_if_0(var int: x, var int: y, var int: p) =
    aux_int_le_zero_if_0(x - y, p);

predicate aux_int_ge_if_0(var int: x, var int: y, var int: p) =
    aux_int_le_zero_if_0(y - x, p);

predicate aux_int_le_if_1(var int: x, var int: y, var int: p) =
    aux_int_le_zero_if_0(x - y, 1 - p);

predicate aux_int_ge_if_1(var int: x, var int: y, var int: p) =
    aux_int_le_zero_if_0(y - x, 1 - p);

predicate aux_int_lt_if_0(var int: x, var int: y, var int: p) =
    aux_int_le_zero_if_0(x - y + 1, p);

predicate aux_int_gt_if_0(var int: x, var int: y, var int: p) =
    aux_int_le_zero_if_0(y - x + 1, p);

predicate aux_int_lt_if_1(var int: x, var int: y, var int: p) =
    aux_int_le_zero_if_0(x - y + 1, 1 - p);


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
