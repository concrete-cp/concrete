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

package cspfj;

import java.io.IOException;
import java.util.Map;

import cspfj.filter.Filter;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

public interface Solver {

    boolean runSolver() throws IOException;

    Map<Variable, Integer> getSolution();

    int getSolutionValue(final int vId);

    float getUserTime();

    int getNbAssignments();

	void setMaxBacktracks(final int maxBacktracks);

	void setUsePrepro(final Class<? extends Filter> filter);

	int getNbSolutions();
    
    int getMaxBacktracks();

	String getXMLConfig();
	
	Problem getProblem() ;
	
	Map<String, Object> getStatistics();
}