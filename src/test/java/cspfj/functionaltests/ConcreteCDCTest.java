package cspfj.functionaltests;

import static org.junit.Assert.*;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URISyntaxException;
import java.sql.SQLException;
import java.text.ParseException;
import java.util.logging.Level;
import java.util.logging.Logger;


import org.junit.Before;
import org.junit.Test;

import cspfj.CspOM;
import cspfj.MGACIter;
import cspfj.Solver;
import cspfj.ResultHandler.Result;
import cspfj.exception.FailedGenerationException;
import cspfj.problem.Problem;

public class ConcreteCDCTest {

    private ResultChecker writer;
    private Concrete concrete;

    @Before
    public void setUp() throws IOException, SQLException, URISyntaxException,
            ClassNotFoundException {
        concrete = new Concrete(new String[] { "-p", "cspfj.filter.DC1", "" });
        writer = new ResultChecker();
        Logger.getLogger("").setLevel(Level.WARNING);
        Logger.getLogger("").getHandlers()[0].setLevel(Level.WARNING);
    }

    @Test
    public void crosswordm1() throws FileNotFoundException, ParseException,
            IOException, FailedGenerationException {

        int nbSol = problem("crossword-m1-debug-05-01.xml", false);
        assertEquals(1, nbSol);
        nbSol = problem("crossword-m1-debug-05-01.xml", true);
        assertEquals(48, nbSol);

    }

    @Test
    public void crosswordm2() throws FileNotFoundException, ParseException,
            IOException, FailedGenerationException {
        assertEquals(1, problem("crossword-m2-debug-05-01.xml", false));
        assertEquals(48, problem("crossword-m2-debug-05-01.xml", true));

    }

    @Test
    public void fapp01_0200_0() throws FileNotFoundException, ParseException,
            IOException, FailedGenerationException {
        assertEquals(0, problem("fapp01-0200-0.xml", false));
        assertEquals(0, problem("fapp01-0200-0.xml", true));

    }

    @Test
    public void queens12() throws FileNotFoundException, ParseException,
            IOException, FailedGenerationException {
        assertEquals(1, problem("queens-12.xml", false));
        assertEquals(14200, problem("queens-12.xml", true));

    }

    @Test
    public void queens12_ext() throws FileNotFoundException, ParseException,
            IOException, FailedGenerationException {
        assertEquals(1, problem("queens-12_ext.xml", false));
        assertEquals(14200, problem("queens-12_ext.xml", true));

    }

    @Test
    public void langford() throws FileNotFoundException, ParseException,
            IOException, FailedGenerationException {
        assertEquals(1, problem("langford-2-4-ext.xml", false));
        assertEquals(2, problem("langford-2-4-ext.xml", true));

    }

    @Test
    public void zebra() throws FileNotFoundException, ParseException,
            IOException, FailedGenerationException {
        assertEquals(1, problem("zebra.xml", false));
        assertEquals(1, problem("zebra.xml", true));

    }

    @Test
    public void bqwh() throws FileNotFoundException, ParseException,
            IOException, FailedGenerationException {
        assertEquals(1, problem("bqwh-15-106-0_ext.xml", false));
        assertEquals(182, problem("bqwh-15-106-0_ext.xml", true));

    }

    @Test
    public void frb35_17_1() throws FileNotFoundException, ParseException,
            IOException, FailedGenerationException {
        assertEquals(1, problem("normalized-frb35-17-1_ext.xml.bz2", false));
        assertEquals(2, problem("normalized-frb35-17-1_ext.xml.bz2", true));
    }

    private int problem(String name, boolean all) throws FileNotFoundException,
            ParseException, IOException, FailedGenerationException {
        cspom.CSPOM cspomProblem = cspom.CSPOM.load(getClass().getResource(
                "../" + name));
        Problem problem = Problem.load(new CspOM(cspomProblem), -1);

        writer.setChecker(cspomProblem);
        final Solver solver = concrete.getSolver(problem, writer);
        ((MGACIter) solver).setAllSolutions(all);
        writer.load(solver, 0);
        Result r = concrete.solve(solver, writer);
        assertNotSame(Result.UNKNOWN, r);
        return solver.getNbSolutions();

    }
}
