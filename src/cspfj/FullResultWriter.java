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
import cspfj.solver.Solver;

public final class FullResultWriter extends ResultHandler {

    public FullResultWriter(int maxTime, boolean displaySolutions)
            throws IOException {
        super(displaySolutions);
        head(maxTime);
    }

    public FullResultWriter(String fileName, int maxTime,
            boolean displaySolutions) throws IOException {
        super(fileName, displaySolutions);
        head(maxTime);
    }

    private void head(final int maxTime) throws IOException {
        writer.write("<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n");
        writer.write("<cspfjResults>\n");
        writer.write("<config>\n");

        writer.write("\t<expire>" + maxTime + "</expire>\n");
        writer.write("</config>\n");
        writer.write("<results>\n");
    }

    /*
     * (non-Javadoc)
     * 
     * @see cspfj.ResultW#problem(java.lang.String)
     */
    @Override
    public void problem(final String name) throws IOException {
        super.problem(name);
        writer.write("\t<problem>\n");
        writer.write("\t\t<name>" + name + "</name>\n");
        writer.flush();
    }

    /*
     * (non-Javadoc)
     * 
     * @see cspfj.ResultW#solver(cspfj.solver.Solver, float)
     */
    @Override
    public void load(final Solver solver, final long load) throws IOException {
        super.load(solver, load);

        writer.write("\t\t<loadTime>" + (load / 1.0e9F) + "</loadTime>\n");
        writer.write("\t\t<solver>" + solver.getClass() + "</solver>\n");
        writer.flush();
    }

    /*
     * (non-Javadoc)
     * 
     * @see cspfj.ResultW#fail(java.lang.Class, java.lang.Throwable, float)
     */
    @Override
    public void fail(final Class solver, final Throwable thrown, final long load)
            throws IOException {
        super.fail(solver, thrown, load);
        writer.write("\t\t<loadTime>" + (load / 1.0e9F) + "</loadTime>\n");
        writer.write("\t\t<solver>" + solver + "</solver>\n");
        writer.write("\t\t<result>" + Result.UNKNOWN + "</result>\n");
        writer.write("\t\t<error>" + thrown + " : " + thrown.getCause()
                + "</error>\n");
        writer.write("\t</problem>\n");
        writer.flush();
    }

    @Override
    public boolean solution(final Map<Variable, Integer> solution,
            final int nbConflicts, final boolean forced) throws IOException {
        if (super.solution(solution, nbConflicts, forced) && nbConflicts > 0) {
            writer.write("\t\t<solution nbConflicts=\"" + nbConflicts + "\">"
                    + solution + "</solution>\n");
            return true;
        }
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see cspfj.ResultW#result(cspfj.ResultWriter.Result, java.lang.Throwable,
     *      int)
     */
    @Override
    public void result(final Result result, final Throwable thrown,
            final long ccks) throws IOException {
        super.result(result, thrown, ccks);

        if (Result.SAT.equals(result)) {
            writer.write("\t\t<solution nbConflicts=\"0\">"
                    + solver.getSolution() + "</solution>\n");
        }
        writer.write("\t\t<result>" + result + "</result>\n");

        if (thrown != null) {
            writer.write("\t\t<error>" + thrown + "</error>\n");
        }

        writer.write("\t\t<statistics>\n");

        writer.write("\t\t\t<cpu>" + solver.getUserTime() + "</cpu>\n");

        writer.write("\t\t\t<checks>" + ccks + "</checks>\n");
        writer
                .write("\t\t\t<assgn>" + solver.getNbAssignments()
                        + "</assgn>\n");
        writer.write("\t\t</statistics>\n");
        writer.write("\t</problem>\n");
        writer.flush();
    }

    /*
     * (non-Javadoc)
     * 
     * @see cspfj.ResultW#close()
     */
    @Override
    public void close() throws IOException {
        writer.write("</results>\n");
        writer.write("<total>\n");
        writer.write("\t<loadTime>" + totalLoad / 1.0e9F + "</loadTime>\n");
        writer.write("\t<cpu>" + totalSolve + "</cpu>\n");

        writer.write("\t<results><sat>" + sat + "</sat><unsat>" + unsat
                + "</unsat><unknown>" + unknown + "</unknown></results>\n");

        writer.write("</total>\n");
        writer.write("</cspfjResults>\n");
        super.close();
    }

}