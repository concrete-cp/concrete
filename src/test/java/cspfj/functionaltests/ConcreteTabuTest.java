package cspfj.functionaltests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;

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
import cspfj.Solver;
import cspfj.ResultHandler.Result;
import cspfj.exception.FailedGenerationException;
import cspfj.problem.Problem;

public class ConcreteTabuTest {

    private ResultChecker writer;
    private Concrete concrete;

    @Before
    public void setUp() throws IOException, SQLException, URISyntaxException,
            ClassNotFoundException {
        concrete = new Concrete(new String[] { "-solver", "Tabu", "" });
        writer = new ResultChecker();
        Logger.getLogger("").setLevel(Level.WARNING);
        Logger.getLogger("").getHandlers()[0].setLevel(Level.WARNING);
    }

//    @Test
//    public void crosswordm1() throws FileNotFoundException, ParseException,
//            IOException, FailedGenerationException {
//
//        assertEquals(1, problem("crossword-m1-debug-05-01.xml"));
//    }

    @Test
    public void crosswordm2() throws FileNotFoundException, ParseException,
            IOException, FailedGenerationException {
        assertEquals(1, problem("crossword-m2-debug-05-01.xml"));
    }

    @Test
    public void queens12() throws FileNotFoundException, ParseException,
            IOException, FailedGenerationException {
        assertEquals(1, problem("queens-12.xml"));
    }

    @Test
    public void queens12_ext() throws FileNotFoundException, ParseException,
            IOException, FailedGenerationException {
        assertEquals(1, problem("queens-12_ext.xml"));
    }

    @Test
    public void langford() throws FileNotFoundException, ParseException,
            IOException, FailedGenerationException {
        assertEquals(1, problem("langford-2-4-ext.xml"));

    }

    @Test
    public void zebra() throws FileNotFoundException, ParseException,
            IOException, FailedGenerationException {
        assertEquals(1, problem("zebra.xml"));

    }

    // @Test
    // public void bqwh() throws FileNotFoundException, ParseException,
    // IOException, FailedGenerationException {
    // assertEquals(1, problem("bqwh-15-106-0_ext.xml"));
    // }

    @Test
    public void frb35_17_1() throws FileNotFoundException, ParseException,
            IOException, FailedGenerationException {
        assertEquals(1, problem("normalized-frb35-17-1_ext.xml.bz2"));
    }

    private int problem(String name) throws FileNotFoundException,
            ParseException, IOException, FailedGenerationException {
        cspom.CSPOM cspomProblem = cspom.CSPOM.load(getClass().getResource(
                "../" + name));
        Problem problem = Problem.load(new CspOM(cspomProblem, -1, false), -1);

        writer.setChecker(cspomProblem);
        final Solver solver = concrete.getSolver(problem, writer);
        writer.load(solver, 0);
        Result r = concrete.solve(solver, writer);
        assertNotSame("Result should be defined", Result.UNKNOWN, r);

        return solver.getNbSolutions();

    }

}
