package cspfj.problem;

import java.io.IOException;

import org.junit.Test;

import cspfj.exception.FailedGenerationException;
import cspfj.filter.AC3;
import cspfj.generator.ProblemGenerator;
import cspom.CSPOM;
import cspom.compiler.ProblemCompiler;
import cspom.xcsp.XCSPParseException;

public class ProblemGeneratorTest {

    @Test
    public void generateTest() throws XCSPParseException, IOException,
            FailedGenerationException, ClassNotFoundException {
        final CSPOM cspom = CSPOM.load(ProblemGeneratorTest.class
                .getResource("fapp01-0200-0.xml"));
        System.out.println(cspom);
        System.out.println("----");
        new ProblemCompiler(cspom).compile();
        System.out.println(cspom);
        System.out.println("----");
        final Problem problem = ProblemGenerator.generate(cspom);
        System.out.println(problem);
        System.out.println("----");
        new AC3(problem).reduceAll();

        System.out.println(problem);
    }
}
