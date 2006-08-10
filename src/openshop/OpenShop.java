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

package openshop;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Filter;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import cspfj.ResultHandler;
import cspfj.constraint.Constraint;
import cspfj.constraint.DTPConstraint;
import cspfj.exception.FailedGenerationException;
import cspfj.exception.OutOfTimeException;
import cspfj.problem.ProblemGenerator;
import cspfj.problem.Variable;
import cspfj.solver.ComboSolver;
import cspfj.solver.Solver;

import ml.options.OptionSet;
import ml.options.Options;
import ml.options.Options.Multiplicity;
import ml.options.Options.Separator;

public class OpenShop implements ProblemGenerator {

    private int[][] durations;

    private final int ub;

    private static final Logger logger = Logger
            .getLogger("cspfj.openshop.OpenShop");

    private int size;

    private Variable[][] variables;

    private final List<Constraint> constraints;

    public OpenShop(final String filename, final int ub) {
        this.ub = ub;
        constraints = new ArrayList<Constraint>();
        try {
            BufferedReader reader = new BufferedReader(new FileReader(filename));

            int ln = 0;
            String line;
            while ((line = reader.readLine()) != null) {
                if (line.charAt(0) == '!') {
                    continue;
                }
                switch (ln) {
                case 0:
                    size = Integer.parseInt(line);
                    durations = new int[size][size];
                    break;
                case 1:
                    break;
                default:
                    String[] values = line.split(" ");
                    for (int i = 0; i < values.length; i++) {
                        durations[ln - 2][i] = Integer.parseInt(values[i]);
                    }

                }
                ln++;
            }

        } catch (FileNotFoundException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

    }

    public int[][] getDurations() {
        return durations;
    }

    public int getUB() {
        return ub;
    }

    public int getSize() {
        return size;
    }

    public static void main(final String[] args) {
        final Options opt = new Options(args, 3);

        opt.addSet("main", 3);

        opt.addOptionAllSets("d", Separator.BLANK, Multiplicity.ZERO_OR_ONE);

        final OptionSet set = opt.getMatchingSet();

        if (set == null) {
            logger
                    .severe("Usage : java cspfj.Cspfj [-e maxTime] [-d level] instance");
            logger.severe(opt.getCheckErrors());
            System.exit(1);
        }

        Logger.getLogger("").getHandlers()[0].setLevel(Level.ALL);

        if (set.isSet("d")) {
            Logger.getLogger("").setLevel(
                    Level.parse((set.getOption("d").getResultValue(0))));
        } else {
            Logger.getLogger("").setLevel(Level.INFO);
            Logger.getLogger("").getHandlers()[0].setFilter(new Filter() {
                public boolean isLoggable(LogRecord arg0) {
                    return arg0.getLoggerName().contains("OpenShop");
                }
            });
        }

        int lb = Integer.parseInt(set.getData().get(1));
        int ub = Integer.parseInt(set.getData().get(2));
        float time = 0;
        do {

             final int test = lb + (ub - lb) / 2;
//            final int test = ub - 1;
            logger.info("TEST : " + test);
            Solver solver = null;
            final OpenShop openShop = new OpenShop(set.getData().get(0), test);
            try {
                solver = new ComboSolver(openShop, new ResultHandler(false));
            } catch (NumberFormatException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            } catch (FailedGenerationException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }

            boolean result = false;
            try {
                result = solver.run(1000000);
            } catch (OutOfTimeException e) {
                logger.severe("Out Of Time");
                System.exit(1);
            } catch (IOException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }

            if (result) {
                ub = openShop.evaluate(solver.getSolution());
            } else {
                lb = test;
            }
            time += solver.getUserTime();
            logger.info(lb + ", " + ub + " (" + solver.getUserTime() + "s)");

        } while (ub - lb > 1);
        logger.info(ub + "! in " + time + "s");
        System.out.println(ub);
    }

    private int evaluate(final Map<Variable, Integer> solution) {
        int evaluation = 0;
        for (Variable v : solution.keySet()) {
            evaluation = Math.max(evaluation, solution.get(v)
                    + durations[v.getId() / size][v.getId() % size]);

        }

        return evaluation;
    }

    public void generate() throws FailedGenerationException {
        final int size = getSize();
        final int ub = getUB();

        variables = new Variable[size][size];
        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
                final int[] domain = new int[ub - durations[i][j] + 1];
                for (int k = 0; k < ub - durations[i][j] + 1; k++) {
                    domain[k] = k;
                }
                variables[i][j] = new Variable(domain);

            }
        }

        // L'op�ration j du job i
        // se fait sur la machine machines[i][j]
        // et dure durations[i][j]
        // Les machines ne font qu'une chose � la fois

        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
                for (int i2 = i + 1; i2 < size; i2++) {
                    constraints.add(new DTPConstraint(new Variable[] {
                            variables[i][j], variables[i2][j] },
                            durations[i][j], durations[i2][j]));
                }

            }
        }

        // cpt = 0 ;

        for (int i = 0; i < size; i++) {
            for (int j = 0; j < (size - 1); j++) {

                for (int j2 = j + 1; j2 < size; j2++) {
                    constraints.add(new DTPConstraint(new Variable[] {
                            variables[i][j], variables[i][j2] },
                            durations[i][j], durations[i][j2]));
                }

            }
        }
    }

    public Variable[] getVariables() {
        final Variable[] vars = new Variable[size * size];
        for (int i = 0; i < size; i++) {
            System.arraycopy(variables[i], 0, vars, i * size, size);
        }
        return vars;
    }

    public Constraint[] getConstraints() {
        return constraints.toArray(new Constraint[constraints.size()]);
    }
}
