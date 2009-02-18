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

package cspfj.filter;

import java.util.Map;

import cspfj.problem.Variable;

/**
 * @author scand1sk
 *
 */
public interface Filter {

    /**
     * @param level Le niveau en cours
     * @return False ssi un domaine a �t� vid�
     */
    boolean reduceAll(int level) throws InterruptedException ;
    
    boolean reduceAfter(int level, Variable variable) ;
    
    Map<String, Object> getStatistics();

    boolean ensureAC();
}