/*
 * Created on 19 mars 2007
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package cspfj.constraint.extension;

import java.util.Arrays;
import java.util.List;

import cspfj.constraint.Constraint;
import cspfj.problem.Variable;

public final class TupleManager {

    private final Constraint constraint;

    private final int[] tuple;

    private final int arity;

    public TupleManager(final Constraint constraint, final int[] tuple) {
        super();
        this.constraint = constraint;
        this.tuple = tuple;
        arity = constraint.getArity();
    }

    public void setFirstTuple() {
        for (int position = arity; --position >= 0;) {
            tuple[position] = constraint.getVariable(position).getFirst();
        }
    }

    public boolean setNextTuple() {
        final int[] tuple = this.tuple;
        for (int i = arity; --i >= 0;) {
            final int index = constraint.getVariable(i).getNext(tuple[i]);

            if (index < 0) {
                tuple[i] = constraint.getVariable(i).getFirst();
            } else {
                tuple[i] = index;
                return true;
            }
        }
        return false;
    }

    public void setFirstTuple(final int variablePosition, final int index) {
        for (int position = arity; --position >= 0;) {
            if (position == variablePosition) {
                tuple[position] = index;
            } else {
                tuple[position] = constraint.getVariable(position).getFirst();
                assert tuple[position] >= 0;
            }
        }

    }

    public boolean setNextTuple(final int fixedVariablePosition) {
        final int[] tuple = this.tuple;

        for (int i = arity; --i >= 0;) {
            if (i == fixedVariablePosition) {
                continue;
            }

            final int index = constraint.getVariable(i).getNext(tuple[i]);

            if (index < 0) {
                tuple[i] = constraint.getVariable(i).getFirst();
            } else {
                tuple[i] = index;
                return true;
            }
        }
        return false;
    }

    public void setFirstTuple(final int[] base) {
        for (int position = arity; --position >= 0;) {
            if (base[position] >= 0) {
                tuple[position] = base[position];
            } else {
                tuple[position] = 0;
            }
        }
    }

    public boolean setNextTuple(final int[] base) {
        for (int i = arity; --i >= 0;) {
            if (base[i] >= 0) {
                continue;
            }

            final int index = tuple[i] + 1;

            if (index >= constraint.getVariable(i).getDomain().maxSize()) {
                tuple[i] = 0;
            } else {
                tuple[i] = index;
                return true;
            }
        }
        return false;
    }

    public void setTuple(final int[] tpl) {
        System.arraycopy(tpl, 0, tuple, 0, arity);
        assert allPresent();
    }

    private boolean allPresent() {
        for (int i = arity; --i >= 0;) {
            if (!constraint.getVariable(i).isPresent(tuple[i])) {
                return false;
            }
        }
        return true;
    }

    public boolean setTupleAfter(final int[] tpl, final int fixed) {
        final int[] tuple = this.tuple;
        System.arraycopy(tpl, 0, tuple, 0, arity);

        // tuple[fixed] = tpl[fixed];
        int changed = arity;
        int pos = 0;
        while (pos < arity) {
            if (pos == fixed) {
                pos++;
                continue;
            }
            // if (changed == arity) {
            // tuple[pos] = tpl[pos];
            // }

            final Variable variable = constraint.getVariable(pos);

            if (pos > changed) {
                tuple[pos++] = variable.getFirst();
            } else {

                int index = tuple[pos];

                if (variable.isPresent(index)) {
                    pos++;
                } else {
                    changed = pos;
                    do {
                        index = variable.getNext(index);
                    } while (index >= 0 && !variable.isPresent(index));

                    if (index < 0) {
                        do {
                            do {
                                pos--;
                            } while (pos == fixed);
                            if (pos < 0) {
                                return false;
                            }
                            tuple[pos] = constraint.getVariable(pos).getNext(
                                    tuple[pos]);
                        } while (tuple[pos] < 0);
                        changed = pos;
                    } else {
                        tuple[pos++] = index;
                    }
                }
            }
        }

        assert allPresent() : Arrays.toString(tpl) + " -> "
                + Arrays.toString(tuple);

        return true;
    }

    public boolean setPrevTuple(final int fixedVariablePosition) {
        final int[] tuple = this.tuple;
        for (int i = arity; --i >= 0;) {
            if (i == fixedVariablePosition) {
                continue;
            }

            final int index = constraint.getVariable(i).getPrev(tuple[i]);

            if (index < 0) {
                tuple[i] = constraint.getVariable(i).getLast();
            } else {
                tuple[i] = index;
                return true;
            }
        }
        return false;
    }

    int[] getTuple() {
        return tuple;
    }
}
