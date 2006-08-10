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

package cspfj.util;

import cspfj.exception.OutOfTimeException;

public class Chronometer {
    private long maxTime;

    private long startTime;

    private long totalTime;
    
    public Chronometer() {
        super();
        maxTime = -1 ;
    }

    public final int getRemainingTime() {
        return Math.round(getRemainingTimeNano() / 1e9f);
    }

    public final long getRemainingTimeNano() {
        return maxTime - getCurrentChronoNano();
    }
    

    public void checkExpiration() throws OutOfTimeException {
        if (maxTime >= 0 && getCurrentChronoNano() > maxTime) {
            throw new OutOfTimeException();
        }
    }

    private void startChrono() {
        startTime = CpuMonitor.getCpuTimeNano();
    }

    public final long getCurrentChronoNano() {
        return CpuMonitor.getCpuTimeNano() - startTime;
    }

    public final float getCurrentChrono() {
        return getCurrentChronoNano() / 1.0e9F;
    }

    public final void validateChrono() {
        totalTime = getCurrentChronoNano();
    }

    public final void setMaxDuration(final int maxDuration) {
        startChrono();
        if (maxDuration >= 0) {
            this.maxTime = getCurrentChronoNano() + (maxDuration * 1000000000L);
        } else {
            maxTime = -1;
        }
    }

    public final float getUserTime() {
        return totalTime / 1.0e9F;
    }
}
