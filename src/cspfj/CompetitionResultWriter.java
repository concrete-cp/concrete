/**
 * CSPFJ - CSP solving API for Java
 * Copyright (C) 2006 Julien VION
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */

package cspfj;

import java.io.IOException;
import java.util.Map;

import cspfj.problem.Variable;

public final class CompetitionResultWriter extends ResultHandler {

    public CompetitionResultWriter(boolean displaySolutions) {
        super(displaySolutions);
    }

    public CompetitionResultWriter(String fileName, boolean displaySolutions)
            throws IOException {
        super(fileName, displaySolutions);
    }

    @Override
    public void fail(final Class solver, final Throwable thrown, final long load)
            throws IOException {
        super.fail(solver, thrown, load);
        writer.write("UNKNOWN\n");
        writer.write(thrown.toString());
        writer.write("\n");
        writer.flush();

    }

    public boolean solution(final Map<Variable, Integer> solution,
            final int nbConflicts, final boolean force) throws IOException {
        if (super.solution(solution, nbConflicts, force)) {
            final StringBuffer stringBuffer = new StringBuffer();
            for (int i = 0; i < solution.size(); i++) {
                for (Variable v : solution.keySet()) {
                    if (v.getId() == i) {
                        stringBuffer.append(solution.get(v)).append(' ');
                        break;
                    }
                }
            }
            if (displaySolutions) {
                writer.write("SOL ");
            }
            writer.write(stringBuffer.toString());
            writer.write("\n");
            writer.flush();
            return true;
        }
        return false;
    }

    @Override
    public void result(final Result result, final Throwable thrown,
            final long ccks) throws IOException {
        super.result(result, thrown, ccks);
        if (displaySolutions) {
            return;
        }
        switch (result) {
        case SAT:
            writer.write("SAT\n");
            break;

        case UNSAT:
            writer.write("UNSAT\n");
            break;

        default:
            writer.write("UNKNOWN\n");
            writer.write(thrown.toString());
            writer.write("\n");
        }

        writer.flush();
    }

}
