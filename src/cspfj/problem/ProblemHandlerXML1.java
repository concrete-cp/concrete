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

import java.util.HashMap;
import java.util.Map;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import cspfj.constraint.Constraint;
import cspfj.constraint.ExtensionConstraint;
import cspfj.exception.FailedGenerationException;

/**
 * @author scand1sk
 * 
 */
public final class ProblemHandlerXML1 extends DefaultHandler implements
		ProblemHandler {

	/**
	 * Liste des domaines.
	 */
	private Map<String, int[]> domains;

	/**
	 * Liste des variables.
	 */
	private Map<String, Variable> variables;

	/**
	 * Liste des relations définissant les contraintes.
	 */
	private Map<String, Relation> relations;

	/**
	 * Liste des contraintes.
	 */
	private Map<String, ExtensionConstraint> constraints;

	/**
	 * Génère variables et contraintes à partir du fichier XML.
	 * 
	 * @see org.xml.sax.ContentHandler#startElement(java.lang.String,
	 *      java.lang.String, java.lang.String, org.xml.sax.Attributes)
	 */
	@Override
	public void startElement(final String uri, final String localName,
			final String qName, final Attributes attributes)
			throws SAXException {

		if (qName == "domains") {

			domains = new HashMap<String, int[]>(Integer.parseInt(attributes
					.getValue("nbDomains")));

		} else if (qName == "domain") {

			final Domain domain = new Domain(Integer.parseInt(attributes
					.getValue("nbValues")));

			final char[] values = attributes.getValue("values").toCharArray();

			domain.addValuesChar(values, 0, values.length);

			try {
				domains.put(attributes.getValue("name"), domain.getValues());
			} catch (FailedGenerationException e) {
				throw new SAXException(e.toString());
			}

		} else if (qName == "variables") {

			variables = new HashMap<String, Variable>(Integer
					.parseInt(attributes.getValue("nbVariables")));

		} else if (qName == "variable") {

			variables.put(attributes.getValue("name"), new Variable(domains
					.get(attributes.getValue("domain"))));

		} else if (qName == "relations") {

			relations = new HashMap<String, Relation>(Integer
					.parseInt(attributes.getValue("nbRelations")));

		} else if (qName == "relation") {

			final boolean supports = attributes.getValue("nbSupports") != null;

			final int nbTuples;

			if (supports) {
				nbTuples = Integer.parseInt(attributes.getValue("nbSupports"));
			} else {
				nbTuples = Integer.parseInt(attributes.getValue("nbConflicts"));
			}

			if (nbTuples < 1) {
				return;
			}

			final int arity = attributes.getValue("domain").split(" ").length;

			final Relation relation = new Relation(arity, nbTuples,
					supports ? "supports" : "conflicts", 1);

			if (supports) {
				relation.addTuplesChar(attributes.getValue("supports")
						.toCharArray(), 0, attributes.getValue("supports")
						.toCharArray().length);
			} else {
				relation.addTuplesChar(attributes.getValue("conflicts")
						.toCharArray(), 0, attributes.getValue("conflicts")
						.toCharArray().length);
			}

			relations.put(attributes.getValue("name"), relation);

		} else if (qName == "constraints") {

			constraints = new HashMap<String, ExtensionConstraint>(Integer
					.parseInt(attributes.getValue("nbConstraints")));

		} else if (qName == "constraint") {

			if (relations.get(attributes.getValue("relation")) == null) {
				return;
			}

			final String[] scopeList = attributes.getValue("scope").split(" ");
			Variable[] scope = new Variable[scopeList.length];
			for (int i = 0; i < scopeList.length; i++) {
				scope[i] = variables.get(scopeList[i]);
			}

			try {
				constraints.put(attributes.getValue("name"),
						new ExtensionConstraint(scope, relations.get(attributes
								.getValue("relation"))));
			} catch (FailedGenerationException e) {
				throw new SAXException(e.toString());
			}
		}

	}

	/**
	 * @return Liste des variables chargées
	 */
	public Variable[] getVariables() {
		return variables.values().toArray(new Variable[variables.size()]);
	}

	/**
	 * @return Liste des contraintes chargées
	 */
	public Constraint[] getConstraints() {
		return constraints.values().toArray(
				new ExtensionConstraint[constraints.size()]);
	}

}
