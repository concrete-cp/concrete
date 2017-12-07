include "lex_lesseq_int.mzn";

predicate lex_less_int(array[int] of var int: x,
                         array[int] of var int: y) = 
                    lex_lesseq_int(x, y) /\	nevec(x,y);
                         