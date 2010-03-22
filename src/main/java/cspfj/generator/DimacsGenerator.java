package cspfj.generator;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

public class DimacsGenerator {
    public static Problem generate(URL dimacs) throws IOException {
        final Reader read = new BufferedReader(new InputStreamReader(dimacs
                .openStream()));

        final Problem problem = new Problem();
        final Map<Integer, Variable> vars = new HashMap<Integer, Variable>();

    }
}
