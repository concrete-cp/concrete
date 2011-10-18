/*
 * Created on 19 mars 2007
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package cspfj.constraint.extension;

import java.util.Arrays;

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
        arity = constraint.arity();
    }

    public void setFirstTuple() {
        for (int position = arity; --position >= 0;) {
            tuple[position] = constraint.scope()[position].dom().first();
        }
    }

    public boolean setNextTuple() {
        for (int i = arity; --i >= 0;) {
            final int index = constraint.scope()[i].dom().next(tuple[i]);

            if (index < 0) {
                tuple[i] = constraint.scope()[i].dom().first();
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
                tuple[position] = constraint.scope()[position].dom().first();
                assert tuple[position] >= 0;
            }
        }

    }

    public boolean setNextTuple(final int fixedVariablePosition) {
        for (int i = arity; --i >= 0;) {
            if (i == fixedVariablePosition) {
                continue;
            }

            final int index = constraint.scope()[i].dom().next(tuple[i]);

            if (index < 0) {
                tuple[i] = constraint.scope()[i].dom().first();
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

            if (index >= constraint.scope()[i].dom().maxSize()) {
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
            if (!constraint.scope()[i].dom().present(tuple[i])) {
                return false;
            }
        }
        return true;
    }

    public boolean setTupleAfter(final int[] tpl, final int fixed) {
        System.arraycopy(tpl, 0, tuple, 0, arity);
        int changed = arity;
        int pos = 0;
        while (pos < arity) {
            if (pos == fixed) {
                pos++;
                continue;
            }

            final Variable variable = constraint.scope()[pos];

            if (pos > changed) {
                tuple[pos++] = variable.dom().first();
            } else {

                int index = tuple[pos];

                if (variable.dom().present(index)) {
                    pos++;
                } else {
                    changed = pos;
                    do {
                        index = variable.dom().next(index);
                    } while (index >= 0 && !variable.dom().present(index));

                    if (index < 0) {
                        do {
                            do {
                                pos--;
                            } while (pos == fixed);
                            if (pos < 0) {
                                return false;
                            }
                            tuple[pos] = constraint.scope()[pos].dom().next(
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
        for (int i = arity; --i >= 0;) {
            if (i == fixedVariablePosition) {
                continue;
            }

            final int index = constraint.scope()[i].dom().prev(tuple[i]);

            if (index < 0) {
                tuple[i] = constraint.scope()[i].dom().last();
            } else {
                tuple[i] = index;
                return true;
            }
        }
        return false;
    }

    public int[] getTuple() {
        return tuple;
    }
}
