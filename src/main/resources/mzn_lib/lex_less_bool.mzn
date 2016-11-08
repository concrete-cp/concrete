include "lex_lesseq_bool.mzn";

predicate lex_less_bool(array[int] of var bool: x,
                         array[int] of var bool: y) = 
                    lex_lesseq_bool(x, y) /\	nevec(x,y);
                         