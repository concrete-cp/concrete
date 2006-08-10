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

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.util.logging.Level;
import java.util.logging.Logger;

import ml.options.OptionSet;
import ml.options.Options;
import ml.options.Options.Multiplicity;
import ml.options.Options.Separator;

import cspfj.ResultHandler.Result;
import cspfj.constraint.AbstractConstraint;
import cspfj.exception.OutOfTimeException;
import cspfj.problem.XMLGenerator.XMLVersion;
import cspfj.solver.MACSolver;
import cspfj.solver.Solver;
import cspfj.util.CpuMonitor;

public final class Cspfj {

    private static final Logger logger = Logger.getLogger("cspfj");

    private static ResultHandler writer;

    // private static boolean maxCSP ;

    private Cspfj() {
        super();
    }

    public static void main(final String[] argv) {
        final Options opt = new Options(argv, 1);
        opt.addSet("competition", 1).addOption("competition");
        opt.addSet("quiet", 1).addOption("q");
        opt.addSet("full", 1);

        opt.addOptionAllSets("exp", Separator.BLANK, Multiplicity.ZERO_OR_ONE);
        opt.addOptionAllSets("d", Separator.BLANK, Multiplicity.ZERO_OR_ONE);
        opt.addOptionAllSets("version", Separator.BLANK,
                Multiplicity.ZERO_OR_ONE);
        opt.addOptionAllSets("solver", Separator.BLANK,
                Multiplicity.ZERO_OR_ONE);
        opt
                .addOptionAllSets("write", Separator.BLANK,
                        Multiplicity.ZERO_OR_ONE);
        opt.addOptionAllSets("max", Multiplicity.ZERO_OR_ONE);

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
            Logger.getLogger("").setLevel(Level.WARNING);
        }

        Constructor<? extends Solver> constructeur = null;

        try {
            final Class<? extends Solver> solver;
            if (set.isSet("solver")) {
                solver = (Class<? extends Solver>) Class.forName(set.getOption(
                        "solver").getResultValue(0));
            } else {
                solver = MACSolver.class;
            }

            constructeur = solver.getConstructor(new Class[] { String.class,
                    XMLVersion.class, ResultHandler.class });
        } catch (ClassNotFoundException e) {
            logger.throwing("Cspfj", "main", e);
            logger.severe(e.toString());
            System.exit(1);
        } catch (SecurityException e) {
            logger.severe(e.toString());
            System.exit(1);
        } catch (NoSuchMethodException e) {
            logger.severe(e.toString());
            System.exit(1);
        }

        final int maxTime = set.isSet("exp") ? Integer.parseInt(set.getOption(
                "exp").getResultValue(0)) : 300;

        final XMLVersion version = set.isSet("version") ? XMLVersion
                .version(Integer.parseInt(set.getOption("version")
                        .getResultValue(0))) : XMLVersion.V2;

        final boolean maxCSP = set.isSet("max");

        try {
            if ("quiet".equals(set.getSetName())) {
                writer = new ResultHandler(maxCSP);

            } else if ("competition".equals(set.getSetName())) {
                writer = set.isSet("write") ? new CompetitionResultWriter(set
                        .getOption("write").getResultValue(0), maxCSP)
                        : new CompetitionResultWriter(maxCSP);

            } else {
                writer = set.isSet("write") ? new FullResultWriter(set
                        .getOption("write").getResultValue(0), maxTime, maxCSP)
                        : new FullResultWriter(maxTime, maxCSP);

            }
            solve(set.getData().get(0), constructeur, maxTime, version);

            writer.close();
        } catch (IOException e) {
            logger.severe(e.toString());
        }

        if (writer.unknown >= 1) {
            System.exit(1);
        }

        System.exit(0);

    }

    public static void solve(final String problem,
            final Constructor<? extends Solver> solverBuilder,
            final int maxTime, final XMLVersion version) throws IOException {

        final File fichier = new File(problem);

        if (fichier.isDirectory()) {

            logger.info("dir : " + problem);
            for (String p : fichier.list()) {
                solve(problem + File.separator + p, solverBuilder, maxTime,
                        version);
            }

        } else {

            writer.problem(problem);

            long loadTime = -CpuMonitor.getCpuTimeNano();

            Solver solver = null;

            try {
                solver = solverBuilder.newInstance(new Object[] { problem,
                        version, writer });

            } catch (OutOfMemoryError e) {
                loadTime += CpuMonitor.getCpuTimeNano();
                writer.fail(solverBuilder.getDeclaringClass(), e, loadTime);
                return;
            } catch (Exception e) {
                loadTime += CpuMonitor.getCpuTimeNano();
                writer.fail(solverBuilder.getDeclaringClass(), e, loadTime);
                return;
            }

            loadTime += CpuMonitor.getCpuTimeNano();
            writer.load(solver, loadTime);

            boolean result = false;
            boolean resolved = false;
            try {
                AbstractConstraint.resetChecks();
                result = solver.run(maxTime);
                resolved = true;
            } catch (OutOfTimeException e) {
                writer.result(Result.UNKNOWN, e, AbstractConstraint
                        .getNbChecks());
                return;
            } catch (OutOfMemoryError e) {
                writer.result(Result.UNKNOWN, e, AbstractConstraint
                        .getNbChecks());
                return;
            } catch (Exception e) {
                writer.fail(solver.getClass(), e, 0);
            }

            if (resolved) {
                if (result) {
                    writer.result(Result.SAT, AbstractConstraint.getNbChecks());
                    writer.solution(solver.getSolution(), 0, true);
                } else {
                    writer.result(Result.UNSAT, AbstractConstraint
                            .getNbChecks());
                }

            } else {
                writer.result(Result.UNKNOWN, AbstractConstraint.getNbChecks());
            }
        }
    }
}
