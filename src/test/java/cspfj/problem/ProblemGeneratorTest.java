package cspfj.problem;

import java.io.IOException;

import org.junit.Test;

import cspfj.exception.FailedGenerationException;
import cspfj.generator.ProblemGenerator;
import cspom.CSPOM;
import cspom.xcsp.XCSPParseException;

public class ProblemGeneratorTest {

	@Test
	public void generateTest() throws XCSPParseException, IOException,
			FailedGenerationException, ClassNotFoundException {
		final CSPOM cspom = CSPOM.load(ProblemGeneratorTest.class
				.getResource("queens-12.xml"));
		final Problem problem = ProblemGenerator.generate(cspom);
		System.out.println(problem);
	}
}
