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
import java.util.logging.Logger;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import cspfj.constraint.AllDifferentConstraint;
import cspfj.constraint.Constraint;
import cspfj.constraint.ExtensionConstraint;
import cspfj.constraint.Predicate;
import cspfj.constraint.PredicateConstraint;
import cspfj.exception.FailedGenerationException;

/**
 * @author scand1sk
 * 
 */
public final class ProblemHandlerXML2 extends DefaultHandler implements
		ProblemHandler {

	/**
	 * Liste des domaines.
	 */
	private Map<String, Domain> domains;

	private Domain currentDomain;

	/**
	 * Liste des variables.
	 */
	private Map<String, Variable> variables;

	/**
	 * Liste des relations définissant les contraintes.
	 */
	private Map<String, Relation> relations;

	private Relation currentRelation;

	private Map<String, Predicate> predicates;

	private Predicate currentPredicate;

	/**
	 * Liste des contraintes.
	 */
	private Map<String, Constraint> constraints;

	private PredicateConstraint currentConstraint;

	private Position position;

	private final static Logger logger = Logger
			.getLogger("cspfj.problem.ProblemHandlerXML2");

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

		if ("domains".equals(qName)) {

			domains = new HashMap<String, Domain>(Integer.parseInt(attributes
					.getValue("nbDomains")));

		} else if ("domain".equals(qName)) {

			position = Position.DOMAIN;

			currentDomain = new Domain(Integer.parseInt(attributes
					.getValue("nbValues")));

			domains.put(attributes.getValue("name"), currentDomain);

		} else if ("variables".equals(qName)) {

			variables = new HashMap<String, Variable>(Integer
					.parseInt(attributes.getValue("nbVariables")));

		} else if ("variable".equals(qName)) {

			try {
				variables.put(attributes.getValue("name"), new Variable(domains
						.get(attributes.getValue("domain")).getValues(),
						attributes.getValue("name")));
			} catch (FailedGenerationException e) {
				throw new SAXException(e.toString());
			}

		} else if ("relations".equals(qName)) {

			relations = new HashMap<String, Relation>(Integer
					.parseInt(attributes.getValue("nbRelations")));

		} else if ("relation".equals(qName)) {

			position = Position.RELATION;

			currentRelation = new Relation(Integer.parseInt(attributes
					.getValue("arity")), Integer.parseInt(attributes
					.getValue("nbTuples")), attributes.getValue("semantics"), 2);

			relations.put(attributes.getValue("name"), currentRelation);

		} else if ("predicates".equals(qName)) {

			predicates = new HashMap<String, Predicate>(Integer
					.parseInt(attributes.getValue("nbPredicates")));

		} else if ("predicate".equals(qName)) {

			try {
				currentPredicate = new Predicate();
			} catch (FailedGenerationException e) {
				throw new SAXException(e.toString());
			}

			predicates.put(attributes.getValue("name"), currentPredicate);

			position = Position.PREDICATE;

		} else if ("parameters".equals(qName)
				&& Position.PREDICATE.equals(position)) {

			position = Position.PREDICATE_PARAMETERS;

		} else if ("expression".equals(qName)
				&& Position.PREDICATE_PARAMETERS.equals(position)) {

			position = Position.PREDICATE_EXPRESSION;

		} else if ("constraints".equals(qName)) {

			constraints = new HashMap<String, Constraint>(Integer
					.parseInt(attributes.getValue("nbConstraints")));

		} else if ("constraint".equals(qName)) {

			final String[] scopeList = attributes.getValue("scope").split(" ");
			Variable[] scope = new Variable[scopeList.length];
			for (int i = 0; i < scopeList.length; i++) {
				scope[i] = variables.get(scopeList[i]);
			}

			final String reference = attributes.getValue("reference");

			if (relations != null && relations.containsKey(reference)) {
				if (relations.get(reference).getNbTuples() > 0) {
					try {
						constraints.put(attributes.getValue("name"),
								new ExtensionConstraint(scope, relations
										.get(reference)));
					} catch (FailedGenerationException e) {
						logger
								.throwing("ProblemHandlerXML2", "startElement",
										e);
						throw new SAXException(e.toString());
					}
				}
			} else if (predicates != null && predicates.containsKey(reference)) {

				position = Position.CONSTRAINT;

				currentConstraint = new PredicateConstraint(scope, predicates
						.get(reference));

				constraints.put(attributes.getValue("name"), currentConstraint);

			} else if ("global:allDifferent".equals(reference)) {
				constraints.put(attributes.getValue("name"),
						new AllDifferentConstraint(scope));
			} else {
				throw new SAXException("Unknown reference " + reference);
			}

		}

	}

	@Override
	public void characters(final char[] ch, final int start, final int length)
			throws SAXException {

		if (position == null) {
			return;
		}

		switch (position) {
		case DOMAIN:
			currentDomain.addValuesChar(ch, start, length);
			break;

		case RELATION:
			currentRelation.addTuplesChar(ch, start, length);
			break;

		case PREDICATE_PARAMETERS:
			currentPredicate.addPredicateParametersChar(ch, start, length);
			break;

		case PREDICATE_EXPRESSION:
			currentPredicate.addExpressionChar(ch, start, length);
			break;

		case CONSTRAINT:
			currentConstraint.addConstraintParametersChar(ch, start, length);
			break;

		default:
		}
	}

	@Override
	public void endElement(final String uri, final String localName,
			final String qName) throws SAXException {
		super.endElement(uri, localName, qName);

		if ("constraint".equals(qName) && Position.CONSTRAINT.equals(position)) {
			try {
				currentConstraint.compileParameters();
			} catch (FailedGenerationException e) {
				logger.throwing("ProblemHandlerXML2", "endElement", e);
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
		return constraints.values().toArray(new Constraint[constraints.size()]);
	}

	private enum Position {
		DOMAIN, RELATION, PREDICATE, PREDICATE_PARAMETERS, PREDICATE_EXPRESSION, CONSTRAINT
	}

}
