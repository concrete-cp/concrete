/*
 * Created on 1 juil. 2005
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package generator;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.logging.Logger;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import ml.options.OptionSet;
import ml.options.Options;
import ml.options.Options.Multiplicity;
import ml.options.Options.Separator;

import org.w3c.dom.Document;
import org.xml.sax.SAXException;

import cspfj.util.SAXErrorHandler;

public class TaillardProblemGenerator {
    // private final int nbJobs;
    //
    // private final int nbOperations;

    // private Seed seed;

    private final int[][] durations;

    private final int[][] machines;

    private final int ub;

    private static final Logger logger = Logger
            .getLogger("cspfj.generator.ProblemGenerator");

    public TaillardProblemGenerator(ProblemType problemType, int instance, int nbJobs,
            int nbOperations) {
        // this.nbJobs = nbJobs;
        // this.nbOperations = nbOperations;

        Document doc = null;

        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();

        // factory.setNamespaceAware(true);
        // factory.setValidating(true);
        // factory.setAttribute(SCHEMA_LANGUAGE, XML_SCHEMA);
        // factory.setAttribute(SCHEMA_SOURCE, schema);

        DocumentBuilder parser;
        try {
            parser = factory.newDocumentBuilder();
            parser.setErrorHandler(new SAXErrorHandler());
            doc = parser.parse("taillard.xml");
        } catch (ParserConfigurationException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (SAXException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        XPath xPath = XPathFactory.newInstance().newXPath();

        String path = "//problem[@id='" + problemType + "']/seedList[@jobs='"
                + nbJobs + "' and @machines='" + nbOperations
                + "']/instance[@no='" + instance + "']/";
        // System.out.println(path);
        int ub = 0;

        int jobSeed = 0;
        int machineSeed = 0;

        logger.info(path) ;
        
        try {
            jobSeed = Integer.parseInt(xPath.evaluate(path + "@time", doc));
            machineSeed = Integer.parseInt(xPath.evaluate(path + "@machine",
                    doc));
            ub = Integer.parseInt(xPath.evaluate(path + "@ub", doc));
        } catch (XPathExpressionException e) {
            e.printStackTrace();
            System.exit(1);
        }

        final BratleyRandomizer random = new BratleyRandomizer(jobSeed);

        this.durations = new int[nbJobs][nbOperations];
        this.machines = new int[nbJobs][nbOperations];

        for (int i = 0; i < nbJobs; i++) {
            for (int j = 0; j < nbOperations; j++) {
                this.durations[i][j] = random.getU(1, 99);

                this.machines[i][j] = j + 1;
            }

        }

        random.reSeed(machineSeed);

        for (int i = 0; i < nbJobs; i++) {
            for (int j = 0; j < nbOperations; j++) {
                int swtch = random.getU(j, nbOperations - 1);
                int temp = this.machines[i][j];
                this.machines[i][j] = this.machines[i][swtch];
                this.machines[i][swtch] = temp;

            }

        }

        this.ub = ub;

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

    public enum ProblemType {
        JOBSHOP, OPENSHOP
    }

    public int[][] getDurations() {
        return durations;
    }

    public int[][] getMachines() {
        return machines;
    }

    public int getLB() {
        int max = 0;

        for (int i = 0; i < durations.length; i++) {
            max = Math.max(max, sum(durations[i]));
        }

        for (int k = 0; k < machines[0].length; k++) {
            int sum = 0;

            for (int i = 0; i < machines.length; i++) {
                for (int j = 0; j < machines[i].length; j++) {
                    if (machines[i][j] == (k + 1)) {
                        sum += durations[i][j];
                    }
                }
            }
            max = Math.max(max, sum);
        }

        return max;
    }

    public int getUB() {
        return ub;
    }

    public static void main(final String[] args) {
        final Options opt = new Options(args, 5);

        opt.addSet("main", 5);

        opt.addOptionAllSets("d", Separator.BLANK, Multiplicity.ZERO_OR_ONE);

        final OptionSet set = opt.getMatchingSet();

        if (set == null) {
            logger
                    .severe("Usage : java cspfj.Cspfj [-e maxTime] [-d level] instance");
            logger.severe(opt.getCheckErrors());
            System.exit(1);
        }

        final int nbJobs = Integer.parseInt(set.getData().get(0));

        final int nbOperations = Integer.parseInt(set.getData().get(1));

        final ProblemType problemType = Boolean.parseBoolean(set.getData().get(
                2)) ? ProblemType.JOBSHOP : ProblemType.OPENSHOP;

//        final boolean controlVariables = Boolean.parseBoolean(set.getData()
//                .get(3));

        final float maxDuration = Float.parseFloat(set.getData().get(3));

        final int numInstance = Integer.parseInt(set.getData().get(4));

        final TaillardProblemGenerator generator = new TaillardProblemGenerator(problemType,
                numInstance, nbJobs, nbOperations);

        final int window = Math.round(generator.getUB() * maxDuration);

        final OutputStreamWriter out = new OutputStreamWriter(System.out);

        try {
            out.write("<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n");
            out.write("<instance>\n");
            out
                    .write("<presentation name=\"?\" maxConstraintArity=\"2\" format=\"XCSP 2.0\" />\n");

            out
                    .write("<domains nbDomains=\"" + nbJobs * nbOperations
                            + "\">\n");
            final int[][] durations = generator.getDurations();
            final int[][] machines = generator.getMachines();

            final int[][] startTimes = new int[nbJobs][nbOperations];

            for (int i = 0; i < nbJobs; i++) {
                for (int j = 0; j < nbOperations; j++) {
                    startTimes[i][j] = nbJobs * i + j;

                    out.write("\t<domain name=\"D" + startTimes[i][j]
                            + "\" nbValues=\"" + (window - durations[i][j] + 1)
                            + "\">0.." + (window - durations[i][j])
                            + "</domain>\n");

                }
            }
            out.write("</domains>\n");

            out.write("<variables nbVariables=\"" + nbJobs * nbOperations
                    + "\">\n");
            for (int i = 0; i < nbJobs; i++) {
                for (int j = 0; j < nbOperations; j++) {
                    out.write("\t<variable name=\"V" + startTimes[i][j]
                            + "\" domain=\"D" + startTimes[i][j] + "\" />\n");
                }
            }
            out.write("</variables>\n");

            out.write("<predicates nbPredicates=\"2\">\n");
            out.write("\t<predicate name=\"P0\">\n");

            out.write("\t\t<parameters>int X0 int X1 int X2</parameters>\n");
            out.write("\t\t<expression>\n");
            out.write("\t\t\t<functional>le(add(X0,X1),X2)</functional>\n");
            out.write("\t\t</expression>\n");
            out.write("\t</predicate>\n");
            out.write("\t<predicate name=\"P1\">\n");
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
            for (int m = 1; m <= nbOperations; m++) {
                // Pour chaque machine
                for (int i = 0; i < nbJobs; i++) {
                    for (int j = 0; j < nbOperations; j++) {
                        // On parcourt la matrice des machines :
                        if (machines[i][j] == m) {
                            for (int i2 = i + 1; i2 < nbJobs; i2++) {
                                for (int j2 = 0; j2 < nbOperations; j2++) {
                                    // On parcourt le reste de la matrice
                                    if (machines[i2][j2] == m)
                                    // && ((i2 != i) || (j2 != j)))
                                    {
                                        sb
                                                .append(
                                                        "\t<constraint name=\"C")
                                                .append(cpt++)
                                                .append(
                                                        "\" arity=\"2\" scope=\"V")
                                                .append(startTimes[i][j])
                                                .append(" V")
                                                .append(startTimes[i2][j2])
                                                .append(
                                                        "\" reference=\"P1\">\n\t\t<parameters>V")
                                                .append(startTimes[i][j])
                                                .append(' ')
                                                .append(durations[i][j])
                                                .append(" V")
                                                .append(startTimes[i2][j2])
                                                .append(" V")
                                                .append(startTimes[i2][j2])
                                                .append(' ')
                                                .append(durations[i2][j2])
                                                .append(" V")
                                                .append(startTimes[i][j])
                                                .append(
                                                        "</parameters>\n\t</constraint>\n");

                                        // if (controlVariables)
                                        // {
                                        // Variable[] involvedVariables2 =
                                        // { startTimes[i][j],
                                        // startTimes[i2][j2],
                                        // machineOrderChoices[cpt++] };
                                        // addConstraint(new
                                        // ConditionalAfter(this,
                                        // involvedVariables2,
                                        // durations[i2][j2]));
                                        // addConstraint(new
                                        // ConditionalBefore(this,
                                        // involvedVariables2,
                                        // durations[i][j]));
                                        //
                                        // }
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // cpt = 0 ;

            // Les op�rations les unes apr�s les autres (Job Shop)
            for (int i = 0; i < nbJobs; i++) {
                for (int j = 0; j < (nbOperations - 1); j++) {

                    if (problemType == ProblemType.JOBSHOP) {
                        sb
                                .append("\t<constraint name=\"C")
                                .append(cpt++)
                                .append("\" arity=\"2\" scope=\"V")
                                .append(startTimes[i][j + 1])
                                .append(" V")
                                .append(startTimes[i][j])
                                .append(
                                        "\" reference=\"P0\">\n\t\t<parameters>V")
                                .append(startTimes[i][j]).append(' ').append(
                                        durations[i][j]).append(" V").append(
                                        startTimes[i][j + 1]).append(
                                        "</parameters>\n\t</constraint>\n");
                    } else {
                        for (int j2 = j + 1; j2 < nbOperations; j2++) {
                            sb
                                    .append("\t<constraint name=\"C")
                                    .append(cpt++)
                                    .append("\" arity=\"2\" scope=\"V")
                                    .append(startTimes[i][j])
                                    .append(" V")
                                    .append(startTimes[i][j2])
                                    .append(
                                            "\" reference=\"P1\">\n\t\t<parameters>V")
                                    .append(startTimes[i][j]).append(' ')
                                    .append(durations[i][j]).append(" V")
                                    .append(startTimes[i][j2]).append(" V")
                                    .append(startTimes[i][j2]).append(' ')
                                    .append(durations[i][j2]).append(" V")
                                    .append(startTimes[i][j]).append(
                                            "</parameters>\n\t</constraint>\n");

                            // if (controlVariables) {
                            // Variable[] involvedVariables2 = {
                            // startTimes[i][j], startTimes[i][j2],
                            // jobOrderChoices[cpt++] };
                            // addConstraint(new ConditionalAfter(this,
                            // involvedVariables2, durations[i][j2]));
                            // addConstraint(new ConditionalBefore(this,
                            // involvedVariables2, durations[i][j]));
                            //
                            // }

                        }

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
