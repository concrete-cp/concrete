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

package sudoku;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import cspfj.ResultHandler;
import cspfj.constraint.AllDifferentConstraint;
import cspfj.constraint.Constraint;
import cspfj.constraint.IffConstraint;
import cspfj.exception.FailedGenerationException;
import cspfj.exception.OutOfTimeException;
import cspfj.problem.ProblemGenerator;
import cspfj.problem.Variable;
import cspfj.solver.ComboSolver;
import cspfj.solver.MACSolver;
import cspfj.solver.Solver;
import cspfj.util.CpuMonitor;
import ml.options.OptionSet;
import ml.options.Options;
import ml.options.Options.Multiplicity;
import ml.options.Options.Separator;

public class Sudoku implements ProblemGenerator {

    private Variable[] variables;

    private Constraint[] constraints;

    private final String sudokuFile;

    private final static Logger logger = Logger.getLogger("sudoku");

    private static int solved = 0;

    private static int failed = 0;

    private static Object[][] primal;

    private static int size;

    // private List<int[]> completeRelation;

    /**
     * @param args
     */
    public static void main(final String[] args) {
        final Options opt = new Options(args, 2);
        opt.addSet("file", 2);
        opt.addSet("stdio", 1).addOption("s");

        opt.addOptionAllSets("e", Separator.BLANK, Multiplicity.ZERO_OR_ONE);
        opt.addOptionAllSets("d", Multiplicity.ZERO_OR_ONE);

        final OptionSet set = opt.getMatchingSet();

        if (set == null) {
            logger.severe("Usage : \n"
                    + "\tjava Sudoku [-d] [-e maxTime] source dest\n"
                    + "\tjava Sudoku [-d] [-e maxTime] -s source");
            System.exit(1);
        }

        if (set.isSet("d")) {
            Logger.getLogger("").setLevel(Level.FINE);

            Logger.getLogger("").getHandlers()[0].setLevel(Level.ALL);
        } else {
            Logger.getLogger("").setLevel(Level.SEVERE);
        }

        final int maxTime;
        if (set.isSet("e")) {
            maxTime = Integer.parseInt(set.getOption("e").getResultValue(0));
        } else {
            maxTime = Integer.MAX_VALUE;
        }

        OutputStreamWriter writer = null;

        if (set.getSetName().equals("stdio")) {
            writer = new OutputStreamWriter(System.out);
        } else {
            try {
                writer = new FileWriter(set.getData().get(1));
            } catch (IOException e) {
                logger.severe("Impossible d'ouvrir le fichier en écriture : "
                        + e.getMessage());
                System.exit(1);
            }
        }

        final float time = solve(set.getData().get(0), maxTime, writer);

        try {
            writer.close();
        } catch (IOException e) {
            logger
                    .severe("Impossible de fermer le fichier : "
                            + e.getMessage());
            System.exit(1);
        }

        logger.info("TOTAL : solved " + solved + " and failed " + failed
                + " in " + time + "s");

    }

    public static float solve(final String problem, final int maxTime,
            final OutputStreamWriter writer) {
        final File fichier = new File(problem);

        float time = 0;
        // System.out.println(problem + "("+problem.length() +")");
        if (fichier.isDirectory()) {

            logger.info("dir : " + problem);
            for (String p : fichier.list()) {

                time += solve(problem + File.separator + p, maxTime, writer);

            }

        } else if (problem.length() >= 4
//                && problem.substring(problem.length() - 4, problem.length())
//                        .equals(".sud")) {
                ){
            logger.info("loading : " + problem);
            Solver solver = null;
            try {

                time = -CpuMonitor.getCpuTime();
                solver = new ComboSolver(new Sudoku(problem), new ResultHandler(false));
                time += CpuMonitor.getCpuTime();
                logger.info("Généré en " + Math.round(time * 1000) + " ms");

            } catch (FailedGenerationException e) {

                time += CpuMonitor.getCpuTime();
                logger.severe("Echec de la génération : " + e.getMessage());
                failed++;
                return time;

            } catch (OutOfMemoryError e) {

                time += CpuMonitor.getCpuTime();
                logger.severe("Echec de la génération : Out of Memory");
                failed++;
                return time;

            }

            boolean result = false;

            try {

                result = solver.run(maxTime - Math.round(time));

            } catch (OutOfTimeException e) {

                logger.severe("Temps imparti dépassé");
                failed++;
                return time + solver.getUserTime();

            } catch (OutOfMemoryError e) {

                logger.severe("Pas assez de mémoire");
                failed++;
                return time + solver.getUserTime();

            } catch (IOException e) {
                
                logger.severe("Erreur d'écriture");
                failed++;
                return time + solver.getUserTime();
            }

            time += solver.getUserTime();

            if (!result) {
                logger.severe("Inconsistance détectée");
                failed++;
                return time;
            }

            logger.info("Résolu en " + solver.getUserTime() + " s ("
                    + solver.getNbAssignments() + " assgn)");
            solved++;

            try {
                writer.write((int) Math.sqrt(size) + "\n");

                int vId = 0;

                for (Object[] i : primal) {

                    for (Object j : i) {
                        if (j instanceof Variable) {
                            writer.write(Integer.toString(solver
                                    .getSolutionValue(vId++)));
                        } else {
                            writer.write(j.toString());
                        }
                        writer.write(' ');
                    }
                    writer.write('\n');

                }
                writer.flush();
            } catch (IOException e) {
                logger.severe("Impossible d'écrire dans le fichier : "
                        + e.getMessage());
                System.exit(1);
            }

        }
        return time;
    }

    public Sudoku(String file) {
        sudokuFile = file;
    }

    public void generate() throws FailedGenerationException {
        try {
            final BufferedReader reader = new BufferedReader(new FileReader(
                    sudokuFile));
            size = (int) Math.pow(Integer.parseInt(reader.readLine()), 2);

            final int[] domain = new int[size];

            for (int i = 0; i < size; i++) {
                domain[i] = i + 1;

            }

            logger.fine("Creating variables");

            primal = new Object[size][size];
            final Variable[][] idxL = new Variable[size][size];
            final Variable[][] idxC = new Variable[size][size];
            final Variable[][] idxB = new Variable[size][size];

            int i = 0;
            for (String line = reader.readLine(); line != null && i < size; line = reader
                    .readLine()) {

                int j = 0;
                for (String variableValue : line.split(" ")) {
                    final int value = Integer.parseInt(variableValue);
                    if (value == 0) {
                        primal[i][j++] = new Variable(domain.clone());

                    } else {
                        primal[i][j++] = value;
                    }
                }
                if (j < size) {
                    throw new FailedGenerationException("Missing variables : "
                            + j + "<" + size);
                }

                i++;
            }

            if (i < size) {
                throw new FailedGenerationException("Missing variables : " + i
                        + "<" + size);
            }

            final List<Variable> allVariables = new ArrayList<Variable>();

            for (i = 0; i < size; i++) {
                for (int j = 0; j < size; j++) {
                    if (primal[i][j] instanceof Variable) {
                        allVariables.add((Variable) primal[i][j]);
                    }
                    allVariables.add(idxL[i][j] = new Variable(domain.clone()));
                    // idxL[i][j].setSelectable(false) ;
                    allVariables.add(idxC[i][j] = new Variable(domain.clone()));
                    // idxC[i][j].setSelectable(false) ;
                    allVariables.add(idxB[i][j] = new Variable(domain.clone()));
                    // idxB[i][j].setSelectable(false) ;
                }
            }

            variables = allVariables.toArray(new Variable[allVariables.size()]);

            final List<Constraint> cons = new ArrayList<Constraint>();

            final Variable[] dualC = new Variable[size];
            final Variable[] dualL = new Variable[size];
            final Variable[] dualB = new Variable[size];

            final List<Variable> primalC = new ArrayList<Variable>();
            final List<Integer> primalCConsts = new ArrayList<Integer>();
            final List<Variable> primalL = new ArrayList<Variable>();
            final List<Integer> primalLConsts = new ArrayList<Integer>();

            logger.fine("Creating Line and Column Constraints");

            for (i = 0; i < size; i++) {
                primalC.clear();
                primalCConsts.clear();
                primalL.clear();
                primalLConsts.clear();
                for (int j = 0; j < size; j++) {
                    if (primal[i][j] instanceof Variable) {
                        primalC.add((Variable) primal[i][j]);
                    } else {
                        primalCConsts.add((Integer) primal[i][j]);
                    }
                    dualC[j] = idxC[j][i];

                    if (primal[j][i] instanceof Variable) {
                        primalL.add((Variable) primal[j][i]);
                    } else {
                        primalLConsts.add((Integer) primal[j][i]);
                    }
                    dualL[j] = idxL[i][j];

                    dualB[j] = idxB[i][j];
                }
                cons.add(new AllDifferentConstraint(primalC
                        .toArray(new Variable[primalC.size()]), primalCConsts
                        .toArray(new Integer[primalCConsts.size()])));
                cons.add(new AllDifferentConstraint(dualC.clone()));
                cons.add(new AllDifferentConstraint(primalL
                        .toArray(new Variable[primalL.size()]), primalLConsts
                        .toArray(new Integer[primalLConsts.size()])));
                cons.add(new AllDifferentConstraint(dualL.clone()));
                cons.add(new AllDifferentConstraint(dualB.clone()));

            }

            final int nbBlocs = (int) Math.sqrt(size);

            final List<Variable> primalB = primalC;
            final List<Integer> primalBConsts = primalCConsts;
            logger.fine("Creating Block Constraints");
            for (i = 0; i < nbBlocs; i++) {

                for (int j = 0; j < nbBlocs; j++) {
                    primalB.clear();
                    primalBConsts.clear();
                    // Variable[] b = new Variable[size] ;
                    for (int k = i * nbBlocs; k < (i + 1) * nbBlocs; k++) {
                        for (int l = j * nbBlocs; l < (j + 1) * nbBlocs; l++) {
                            // System.out.print((size * k + l) + " ");
                            if (primal[k][l] instanceof Variable) {
                                primalB.add((Variable) primal[k][l]);
                            } else {
                                primalBConsts.add((Integer) primal[k][l]);
                            }

                        }

                    }
                    cons.add(new AllDifferentConstraint(primalB
                            .toArray(new Variable[primalB.size()]),
                            primalBConsts.toArray(new Integer[primalBConsts
                                    .size()])));
                }
            }
            logger.fine("Creating IFF Constraints");
            for (i = 0; i < size; i++) {
                for (int j = 0; j < size; j++) {
                    for (int v = 0; v < size; v++) {
                        if (primal[i][j] instanceof Variable) {
                            cons.add(new IffConstraint(new Variable[] {
                                    idxL[j][v], (Variable) primal[i][j] },
                                    i + 1, v + 1));

                        } else if ((Integer) primal[i][j] == v + 1) {
                            idxL[j][v].makeSingleton(i + 1, 0);
                        }

                        if (primal[i][j] instanceof Variable) {
                            cons.add(new IffConstraint(new Variable[] {
                                    idxC[i][v], (Variable) primal[i][j] },
                                    j + 1, v + 1));
                        } else if ((Integer) primal[i][j] == v + 1) {
                            idxC[i][v].makeSingleton(j + 1, 0);
                        }

                        if (primal[i][j] instanceof Variable) {
                            cons
                                    .add(new IffConstraint(new Variable[] {
                                            idxB[nbBlocs * (i / nbBlocs) + j
                                                    / nbBlocs][v],
                                            (Variable) primal[i][j] },
                                            nbBlocs * (i % nbBlocs)
                                                    + (j % nbBlocs) + 1, v + 1));
                        } else if ((Integer) primal[i][j] == v + 1) {
                            idxB[nbBlocs * (i / nbBlocs) + j / nbBlocs][v]
                                    .makeSingleton(nbBlocs * (i % nbBlocs)
                                            + (j % nbBlocs) + 1, 0);

                            // System.out.println(i+"/"+nbBlocs+"="+(i/nbBlocs));
                        }
                    }
                }
            }

            constraints = cons.toArray(new Constraint[cons.size()]);

            logger.fine("Done");

        } catch (Exception e) {
            logger.info(Arrays.toString(e.getStackTrace()));
            throw new FailedGenerationException(e.toString());

        }

    }

    public Variable[] getVariables() {
        return variables;
    }

    public Constraint[] getConstraints() {
        return constraints;
    }

}
