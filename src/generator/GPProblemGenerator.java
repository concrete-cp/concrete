/*
 * Created on 1 juil. 2005
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package generator;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.logging.Logger;


import ml.options.OptionSet;
import ml.options.Options;
import ml.options.Options.Multiplicity;
import ml.options.Options.Separator;

public class GPProblemGenerator {
    // private final int nbJobs;
    //
    // private final int nbOperations;

    // private Seed seed;

    private int[][] durations;

    private final int ub;

    private static final Logger logger = Logger
            .getLogger("cspfj.generator.ProblemGenerator");

    private int size;

    public GPProblemGenerator(final String filename, final int ub) {
        this.ub = ub;

        try {
            BufferedReader reader = new BufferedReader(new FileReader(filename));

            int ln = 0;
            String line;
            while ((line = reader.readLine()) != null) {
                if (line.charAt(0) == '!') {
                    continue;
                }
                switch (ln) {
                case 0:
                    size = Integer.parseInt(line);
                    durations = new int[size][size];
                    break;
                case 1:
                    break;
                default:
                    String[] values = line.split(" ");
                    for (int i = 0; i < values.length; i++) {
                        durations[ln - 2][i] = Integer.parseInt(values[i]);
                    }

                }
                ln++;
            }

        } catch (FileNotFoundException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

    }

    private static int sum(final int[] a) {
        int s = 0;

        for (int i = 0; i < a.length; i++) {
            s += a[i];
        }

        return s;
    }

    // public String toString() {
    // StringBuffer sb = new StringBuffer();
    //
    // sb.append("<timeSeed>").append(seed.job).append("</timeSeed>\n");
    // sb.append("<timeMatrix>\n");
    // appendMatrix(sb, this.durations);
    // sb.append("</timeMatrix>\n");
    // sb.append("<machineSeed>").append(seed.machine).append(
    // "</machineSeed>\n");
    // sb.append("<machineMatrix>");
    // appendMatrix(sb, this.machines);
    // sb.append("</machineMatrix>\n");
    //
    // sb.append("<lb>").append(this.lb).append("</lb>\n");
    // sb.append("<ub>").append(this.ub).append("</ub>\n");
    //
    // return sb.toString();
    // }

    // private void appendMatrix(StringBuffer sb, int[][] matrix) {
    // for (int[] matrix1 : matrix) {
    // for (int m : matrix1) {
    // sb.append(m + " ");
    // }
    // sb.append("\n");
    // }
    //
    // }

    public int[][] getDurations() {
        return durations;
    }

    public int getLB() {
        int max = 0;

        for (int i = 0; i < size; i++) {
            max = Math.max(max, sum(durations[i]));
        }

        for (int i = 0; i < size; i++) {
            int sum = 0;

            for (int j = 0; j < size; i++) {
                sum += durations[j][i];
            }
            max = Math.max(max, sum);
        }

        return max;
    }

    public int getUB() {
        return ub;
    }

    public int getSize() {
        return size;
    }

    public static void main(final String[] args) {
        final Options opt = new Options(args, 2);

        opt.addSet("main", 2);

        opt.addOptionAllSets("d", Separator.BLANK, Multiplicity.ZERO_OR_ONE);

        final OptionSet set = opt.getMatchingSet();

        if (set == null) {
            logger
                    .severe("Usage : java cspfj.Cspfj [-e maxTime] [-d level] instance");
            logger.severe(opt.getCheckErrors());
            System.exit(1);
        }
        final GPProblemGenerator generator = new GPProblemGenerator(set
                .getData().get(0), Integer.parseInt(set.getData().get(1)));
        final int size = generator.getSize();
        final int ub = generator.getUB();
        final OutputStreamWriter out = new OutputStreamWriter(System.out);

        try {
            out.write("<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n");
            out.write("<instance>\n");
            out
                    .write("<presentation name=\"?\" maxConstraintArity=\"2\" format=\"XCSP 2.0\" />\n");

            out.write("<domains nbDomains=\"" + size * size + "\">\n");
            final int[][] durations = generator.getDurations();

            final int[][] startTimes = new int[size][size];

            for (int i = 0; i < size; i++) {
                for (int j = 0; j < size; j++) {
                    startTimes[i][j] = size * i + j;

                    out
                            .write("\t<domain name=\"D" + startTimes[i][j]
                                    + "\" nbValues=\""
                                    + (ub - durations[i][j] + 1) + "\">0.."
                                    + (ub - durations[i][j]) + "</domain>\n");

                }
            }
            out.write("</domains>\n");

            out.write("<variables nbVariables=\"" + size * size + "\">\n");
            for (int i = 0; i < size; i++) {
                for (int j = 0; j < size; j++) {
                    out.write("\t<variable name=\"V" + startTimes[i][j]
                            + "\" domain=\"D" + startTimes[i][j] + "\" />\n");
                }
            }
            out.write("</variables>\n");

            out.write("<predicates nbPredicates=\"1\">\n");
            out.write("\t<predicate name=\"P0\">\n");
            out
                    .write("\t\t<parameters>int X0 int X1 int X2 int X3 int X4 int X5</parameters>\n");
            out.write("\t\t<expression>\n");
            out
                    .write("\t\t\t<functional>or(le(add(X0,X1),X2),le(add(X3,X4),X5))</functional>\n");
            out.write("\t\t</expression>\n");
            out.write("\t</predicate>\n");

            out.write("</predicates>\n");

            final StringBuffer sb = new StringBuffer();

            int cpt = 0;

            // L'op�ration j du job i
            // se fait sur la machine machines[i][j]
            // et dure durations[i][j]
            // Les machines ne font qu'une chose � la fois

            for (int i = 0; i < size; i++) {
                for (int j = 0; j < size; j++) {
                    for (int i2 = i + 1; i2 < size; i2++) {

                        sb
                                .append("\t<constraint name=\"C")
                                .append(cpt++)
                                .append("\" arity=\"2\" scope=\"V")
                                .append(startTimes[i][j])
                                .append(" V")
                                .append(startTimes[i2][j])
                                .append(
                                        "\" reference=\"P0\">\n\t\t<parameters>V")
                                .append(startTimes[i][j]).append(' ').append(
                                        durations[i][j]).append(" V").append(
                                        startTimes[i2][j]).append(" V").append(
                                        startTimes[i2][j]).append(' ').append(
                                        durations[i2][j]).append(" V").append(
                                        startTimes[i][j]).append(
                                        "</parameters>\n\t</constraint>\n");

                    }

                }
            }

            // cpt = 0 ;

            for (int i = 0; i < size; i++) {
                for (int j = 0; j < (size - 1); j++) {

                    for (int j2 = j + 1; j2 < size; j2++) {
                        sb
                                .append("\t<constraint name=\"C")
                                .append(cpt++)
                                .append("\" arity=\"2\" scope=\"V")
                                .append(startTimes[i][j])
                                .append(" V")
                                .append(startTimes[i][j2])
                                .append(
                                        "\" reference=\"P0\">\n\t\t<parameters>V")
                                .append(startTimes[i][j]).append(' ').append(
                                        durations[i][j]).append(" V").append(
                                        startTimes[i][j2]).append(" V").append(
                                        startTimes[i][j2]).append(' ').append(
                                        durations[i][j2]).append(" V").append(
                                        startTimes[i][j]).append(
                                        "</parameters>\n\t</constraint>\n");

                    }

                }
            }
            out.write("<constraints nbConstraints=\"" + cpt + "\">\n");
            out.write(sb.toString());
            out.write("</constraints>\n");

            out.write("</instance>\n");
            out.close();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

    }
}
