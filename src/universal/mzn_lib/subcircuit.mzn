include "alldifferent.mzn";

predicate fzSubcircuit(array[int] of var int: x);

/** @group globals
  Constrains the elements of \a x to define a subcircuit where \a x[\p i] = \p j
  means that \p j is the successor of \p i and \a x[\p i] = \p i means that \p i
  is not in the circuit.
*/
predicate subcircuit(array[int] of var int: x) = 
    alldifferent(x) /\
    fzSubcircuit(x);

predicate subcircuit_reif(array[int] of var int: x, var bool: b) = 
    abort("Reified subcircuit/1 is not supported.");

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
