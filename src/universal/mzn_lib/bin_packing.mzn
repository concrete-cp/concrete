include "bin_packing_load.mzn";

predicate bin_packing(par int: c,
                           array[int] of var int: bin,
                           array[int] of par int: w) =
             let { array[dom_array(bin)] of var 0..c: load } in 
              bin_packing_load(load, bin, w);