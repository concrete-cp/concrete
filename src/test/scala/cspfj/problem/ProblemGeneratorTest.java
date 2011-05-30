package cspfj.problem;

import java.io.IOException;
import java.util.logging.Logger;

import org.junit.Test;

import cspfj.exception.FailedGenerationException;
import cspfj.filter.AC3Constraint;
import cspfj.generator.ProblemGenerator;
import cspom.CSPOM;
import cspom.CSPParseException;
import cspom.compiler.ProblemCompiler;

public final class ProblemGeneratorTest {
    private static final Logger LOGGER = Logger
            .getLogger(ProblemGeneratorTest.class.getName());

    @Test
    public void zebra() throws CSPParseException, IOException,
            FailedGenerationException, ClassNotFoundException {
        generateTest("zebra.xml");
    }

    @Test
    public void queens4() throws CSPParseException, IOException,
            FailedGenerationException, ClassNotFoundException {
        generateTest("queens-4.xml");
    }

    @Test
    public void queens12() throws CSPParseException, IOException,
            FailedGenerationException, ClassNotFoundException {
        generateTest("queens-12.xml");
    }

    @Test
    public void queens20() throws CSPParseException, IOException,
            FailedGenerationException, ClassNotFoundException {
        generateTest("queens-20.xml.bz2");
    }

    @Test
    public void queens50() throws CSPParseException, IOException,
            FailedGenerationException, ClassNotFoundException {
        generateTest("queens-50.xml.bz2");
    }

    @Test
    public void scen11f10() throws CSPParseException, IOException,
            FailedGenerationException, ClassNotFoundException {
        generateTest("scen11-f10.xml.bz2");
    }

    @Test
    public void crosswordm2() throws CSPParseException, IOException,
            FailedGenerationException, ClassNotFoundException {
        generateTest("crossword-m2-debug-05-01.xml");
    }

    @Test
    public void lexHerald() throws CSPParseException, IOException,
            FailedGenerationException, ClassNotFoundException {
        generateTest("normalized-crossword-m1-lex-15-04.xml.bz2");
    }

    // @Test
    // public void fapp01_0200_0() throws CSPParseException, IOException,
    // FailedGenerationException, ClassNotFoundException {
    // generateTest("fapp01-0200-0.xml");
    // }

    private void generateTest(final String file) throws CSPParseException,
            IOException, FailedGenerationException, ClassNotFoundException {
        final CSPOM cspom = CSPOM.load(ProblemGeneratorTest.class
                .getResource(file));
        {
            final StringBuilder stb = new StringBuilder();
            stb.append(cspom).append('\n');
            stb.append(cspom.getVariables().size()).append(" vars, ")
                    .append(cspom.getConstraints().size()).append(" cons");
            LOGGER.info(stb.toString());
        }

        ProblemCompiler.compile(cspom);
        {
            final StringBuilder stb = new StringBuilder();
            stb.append(cspom).append('\n');
            stb.append(cspom.getVariables().size()).append(" vars, ")
                    .append(cspom.getConstraints().size()).append(" cons");
            LOGGER.info(stb.toString());
        }

        final Problem problem = ProblemGenerator.generate(cspom);
        {
            final StringBuilder stb = new StringBuilder();
            stb.append(problem).append('\n');
            stb.append(problem.getVariables().length).append(" vars, ")
                    .append(problem.getConstraints().size()).append(" cons");
            LOGGER.info(stb.toString());
        }
        new AC3Constraint(problem).reduceAll();

        LOGGER.info(problem.toString());
    }
}
