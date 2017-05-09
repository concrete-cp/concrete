include "bin_packing_load.mzn";

predicate bin_packing_capa(array[int] of par int: capa,
                           array[int] of var int: bin,
                           array[int] of int: w) =
             let { array[index_set(capa)] of var int: load } in
              forall(i in index_set(capa))(0 <= load[i] /\ load[i] <= capa[i]) /\ 
              bin_packing_load(load, bin, w);