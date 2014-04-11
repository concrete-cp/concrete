package abscon.instance.tools;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.LinkedList;
import java.util.List;
import java.util.StringTokenizer;

import abscon.instance.InstanceTokens;
import abscon.instance.components.PConstraint;
import abscon.instance.components.PVariable;

/**
 * @author christophe lecoutre
 * @version 2.1.1
 */
public class SolutionChecker {

	private InstanceParser parser;

	private InstanceParser loadAndParseInstance(String instanceFileName) throws Exception {
		InstanceParser parser = new InstanceParser();
		parser.loadInstance(instanceFileName);
		parser.parse(false);
		return parser;
	}

	private int[] buildSolutionFrom(String line, int nbVariables) throws Exception {
		int[] t = new int[nbVariables];
		int i = 0;
		StringTokenizer st = new StringTokenizer(line);
		while (st.hasMoreTokens()) {
			String token = st.nextToken();
			int last = token.length() - 1;
			while (last >= 0 && Character.isDigit(token.charAt(last)))
				last--;
			if (last >= 0 && token.charAt(last) == '-')
				last--;
			t[i++] = Integer.parseInt(token.substring(last + 1));
		}
		if (i < nbVariables)
			throw new IllegalArgumentException();
		return t;
	}

	private int[] loadSolution(String solutionFileName, int nbVariables) throws Exception {
		BufferedReader rd = new BufferedReader(new FileReader(solutionFileName));
		String line = rd.readLine().trim();
		if (line.equals(InstanceTokens.UNSAT)) {
			System.err.println("PROBLEM \t The file " + solutionFileName + " does not contain any solution.");
			System.exit(2);
		}
		if (line.equals(InstanceTokens.SAT)) {
			line = rd.readLine().trim();
			return buildSolutionFrom(line, nbVariables);
		}
		String previousLine = null;
		while (line != null && line.startsWith(InstanceTokens.SOL)) {
			previousLine = line;
			line = rd.readLine().trim();
		}
		if (previousLine != null)
			return buildSolutionFrom(previousLine, nbVariables);
		return buildSolutionFrom(line, nbVariables);
	}

	private void dealWithInstanceFileName(String instanceFileName) {
		if (!new File(instanceFileName).exists()) {
			System.err.println("PROBLEM \t The file " + instanceFileName + " has not been found.");
			System.exit(2);
		}
		try {
			parser = loadAndParseInstance(instanceFileName);
		} catch (Exception e) {
			System.err.println("PROBLEM \t When loading and/or parsing file " + instanceFileName + " " + e);
			e.printStackTrace();
			System.exit(2);
		}
	}

	private int[] dealWithSolutionFileName(String solutionFileName) {
		try {
			if (!new File(solutionFileName).exists()) {
				System.err.println("The file " + solutionFileName + " has not been found");
				System.exit(2);
			}
			try {
				return loadSolution(solutionFileName, parser.getVariables().length);
			} catch (Exception e) {
				System.err.println("PROBLEM \t When loading and/or parsing file " + solutionFileName + " " + e);
				System.exit(2);
				return null;
			}
		} catch (Throwable e) {
			System.err.println("PROBLEM \t " + e.getMessage());
			System.exit(2);
			return null;
		}
	}

	/**
	 * Returns -1 if the solution is valid, the position of ther first invalid value otherwise
	 */
	public int isSolutionValid(int[] solution) {
		assert parser.getVariables().length == solution.length;
		for (int i = 0; i < solution.length; i++)
			if (!parser.getVariables()[i].getDomain().contains(solution[i]))
				return i;
		return -1;
	}

	/**
	 * Extract from the given solution the tuple of values that corresponds to the scope of the given constraint. <br>
	 * Of course, a more efficient approach could be devised, but here efficiency does not seem very important.
	 */
	private int[] buildTupleFor(PConstraint constraint, int[] solution) {
		int[] tuple = new int[constraint.getScope().length];
		PVariable[] involvedVariables = constraint.getScope();
		for (int i = 0; i < involvedVariables.length; i++) {
			int position = 0;
			while (involvedVariables[i] != parser.getVariables()[position])
				position++;
			tuple[i] = solution[position];
		}
		return tuple;
	}

	private void checkSolution(int[] solution) {
		if (parser.getVariables().length != solution.length) {
			System.err.println("PROBLEM \t The number of variables is " + parser.getVariables().length + " while the size of the solution is " + solution.length);
			System.exit(2);
		}

		int invalidPosition = isSolutionValid(solution);
		if (invalidPosition != -1) {
			System.out.println("ERROR \t The given solution is not valid as the " + invalidPosition + "th value of the solution is not present in the domain of the associated variable");
			System.exit(1);
		}

		List<String> list = new LinkedList<String>();
		long sum = 0;
		for (PConstraint constraint : parser.getMapOfConstraints().values()) {
			long cost = constraint.computeCostOf(buildTupleFor(constraint, solution));
			if (cost > 0)
				list.add(constraint.getName());
			sum += cost;
		}

		System.out.println("solutionCost " + sum);
		System.out.println("listOfUnsatisfiedConstraints " + list);
		System.exit(0);
	}

	public SolutionChecker(String instanceFileName) {
		dealWithInstanceFileName(instanceFileName);
		String s0 = "satisfiable " + parser.getSatisfiable() + "  minViolatedConstraints " + parser.getMinViolatedConstraints();
		String s1 = "\t nbVariables " + parser.getVariables().length + "  nbConstraints " + parser.getMapOfConstraints().size();
		String s2 = "\t maxConstraintArity " + parser.getMaxConstraintArity() + "  nbExtensionConstraints " + parser.getNbExtensionConstraints() + "  nbIntensionConstraints "
				+ parser.getNbIntensionConstraints() + "  nbGlobalConstraints " + parser.getNbGlobalConstraints();
		System.out.println(s0 + s1 + s2);
		System.exit(0);
	}

	public SolutionChecker(String instanceFileName, String solutionFileName) {
		dealWithInstanceFileName(instanceFileName);
		checkSolution(dealWithSolutionFileName(solutionFileName));
	}

	public SolutionChecker(String instanceFileName, int[] solution) {
		dealWithInstanceFileName(instanceFileName);
		checkSolution(solution);
	}

	public static void main(String[] args) {
		try {
			if (args.length == 1)
				new SolutionChecker(args[0]);
			else if (args.length == 2)
				new SolutionChecker(args[0], args[1]);
			else if (args.length > 2) {
				int[] solution = new int[args.length - 1];
				for (int i = 0; i < solution.length; i++) {
					try {
						solution[i] = Integer.parseInt(args[i + 1]);
					} catch (NumberFormatException e) {
						System.err.println("PROBLEM \t With the given solution: " + args[i + 1] + " is not an integer " + e);
						System.exit(2);
					}
				}
				new SolutionChecker(args[0], solution);
			} else {
				System.out.println();
				System.out.println("SolutionChecker " + InstanceParser.VERSION);
				System.out.println("Usage 1: java ... SolutionChecker <instanceFileName>");
				System.out.println("Usage 2: java ... SolutionChecker <instanceFileName> <solutionFileName>");
				System.out.println("Usage 3: java ... SolutionChecker <instanceFileName> <solution>");
				System.out.println();
				System.out.println("  <instanceFileName> must be the name of a file which contains the representation of a CSP or WCSP instance in format XCSP 2.1.");
				System.out.println("  <solutionFileName> must be the name of a file which:");
				System.out.println("     - either contains on the first line a sequence of values (integers separated by whitespace(s)), one for each variable of the instance");
				System.out.println("     - or respects the output format of the 2008 CSP competition");
				System.out.println("  <solution> must be a sequence of values (integers separated by whitespace(s)), one for each variable of the instance");
				System.out.println();
				System.out.println("With Usage 1, SolutionChecker outputs some information about the given instance");
				System.out.println("With Usage 2 and Usage 3, SolutionChecker outputs the cost of the given solution (number of violated constraints for CSP)");
				System.out.println();
				System.out.println("Exit code of solutionChecker is as follows:");
				System.out.println("  0 : no problem occurs and the solution is valid");
				System.out.println("  1 : the solution is not valid");
				System.out.println("  2 : a problem occurs (file not found, ...)");
				System.exit(0);
			}
		} catch (Throwable e) {
			System.err.println("PROBLEM \t " + e.getMessage());
			System.exit(2);
		}
	}
}
