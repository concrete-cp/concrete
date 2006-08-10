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

import cspfj.exception.FailedGenerationException;

public final class Domain {

    private final int nbValues;

    private final StringBuffer stringBuffer;

    public Domain(final int nbValues) {
        super();

        this.nbValues = nbValues;

        stringBuffer = new StringBuffer();
    }

    public void addValuesChar(final char[] ch, final int start, final int length) {
        stringBuffer.append(ch, start, length);
    }

    public int[] getValues() throws FailedGenerationException {
        final int[] values = new int[nbValues];

        final String[] listOfValues = stringBuffer.toString().trim().split(" ");
        int offset = 0;
        for (String currentValue : listOfValues) {
            if (currentValue.contains("..")) {
                
                final String[] fromto = currentValue.split("\\.\\.");
                final int from = Integer.parseInt(fromto[0]);
                final int to = Integer.parseInt(fromto[1]);
                for (int i = 0; i <= to - from; i++) {
                    values[offset + i] = i + from;
                }
                offset += 1 + to - from;

            } else {

                values[offset++] = Integer.parseInt(currentValue);
                
            }
        }
        
        if (offset < nbValues) {
            throw new FailedGenerationException("Inconsistent number of values") ; 
        }
        
        return values ;
    }

}
