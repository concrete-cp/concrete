package cspfj.problem;

import java.io.IOException;

import org.junit.Test;

import cspfj.exception.FailedGenerationException;
import cspfj.filter.AC3;
import cspfj.generator.ProblemGenerator;
import cspom.CSPOM;
import cspom.xcsp.XCSPParseException;

public class ProblemGeneratorTest {

    @Test
    public void generateTest() throws XCSPParseException, IOException,
            FailedGenerationException, ClassNotFoundException {
        final CSPOM cspom = CSPOM.load(ProblemGeneratorTest.class
                .getResource("queens-4.xml"));
        final Problem problem = ProblemGenerator.generate(cspom);
        System.out.println(problem);
        System.out.println("----");
        new AC3(problem).reduceAll();

        System.out.println(problem);
    }
}
