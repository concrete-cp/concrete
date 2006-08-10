/**
 * CSPFJ - CSP solving API for Java
 * Copyright (C) 2006 Julien VION
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */

package cspfj.problem;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.logging.Logger;
import java.util.zip.GZIPInputStream;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.InputSource;
import org.xml.sax.XMLReader;

import cspfj.constraint.Constraint;
import cspfj.exception.FailedGenerationException;

public final class XMLGenerator implements ProblemGenerator {

    private final String problemName;

    private Variable[] variables;

    private Constraint[] constraints;

    private final static Logger logger = Logger
            .getLogger("cspfj.problem.XMLGenerator");

    private final XMLVersion version;

    public XMLGenerator(String problem, XMLVersion version) {
        this.problemName = problem;
        this.version = version;
    }

    public void generate() throws FailedGenerationException {

        final InputStream problemInputStream;

        if (problemName.substring(problemName.length() - 3,
                problemName.length()).equals(".gz")) {

            try {
                problemInputStream = new GZIPInputStream(new FileInputStream(
                        problemName));
            } catch (Exception e) {
                throw new FailedGenerationException(e.toString());
            }

        } else {

            try {
                problemInputStream = new FileInputStream(problemName);
            } catch (FileNotFoundException e) {
                throw new FailedGenerationException(e.toString());
            }

        }

        final SAXParserFactory saxParserFactory = SAXParserFactory
                .newInstance();
        final SAXParser saxParser;
        final XMLReader reader;
        try {
            saxParser = saxParserFactory.newSAXParser();
            reader = saxParser.getXMLReader();
        } catch (Exception e) {
            logger.throwing("XMLGenerator", "generate", e);
            throw new FailedGenerationException(e.toString());
        }

        final ProblemHandler handler;

        switch (version) {
        case V1:
            handler = new ProblemHandlerXML1();
            break;

        default:
            handler = new ProblemHandlerXML2();
        }
        reader.setContentHandler(handler);

        try {
            reader.parse(new InputSource(problemInputStream));
        } catch (Exception e) {
            logger.throwing("XMLGenerator", "generate", e);
            throw new FailedGenerationException(e.toString());
        }

        variables = handler.getVariables();
        constraints = handler.getConstraints();
    }

    public Variable[] getVariables() {
        return variables;
    }

    public Constraint[] getConstraints() {
        return constraints;
    }

    public enum XMLVersion {
        V1, V2;

        public static XMLVersion version(final int version) {
            switch (version) {
            case 1:
                return V1;

            default:
                return V2;

            }
        }
    }

}
