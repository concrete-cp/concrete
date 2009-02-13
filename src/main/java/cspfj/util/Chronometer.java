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

import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;

public final class Chronometer {
//    private long maxTime;
    private final static ThreadMXBean threadMXBean ;

    static {
        threadMXBean = ManagementFactory.getThreadMXBean()  ;
        threadMXBean.setThreadCpuTimeEnabled(true) ;
    }
    
    private long startTime;

    private long totalTime;
    
    private long threadId ;


//    public final int getRemainingTime() {
//        return Math.round(getRemainingTimeNano() / 1e9f);
//    }

//    public final long getRemainingTimeNano() {
//        return maxTime - getCurrentChronoNano();
//    }
//    
//
//    public void checkExpiration() throws OutOfTimeException {
//        if (maxTime >= 0 && getCurrentChronoNano() > maxTime) {
//            throw new OutOfTimeException();
//        }
//    }

    public void startChrono() {
        startTime = threadMXBean.getCurrentThreadUserTime();
        threadId = Thread.currentThread().getId();
    }

    public long getCurrentChronoNano() {
        return threadMXBean.getThreadCpuTime(threadId) - startTime;
    }
    
    

    public float getCurrentChrono() {
        return getCurrentChronoNano() / 1.0e9F;
    }

    public void validateChrono() {
        totalTime = getCurrentChronoNano();
    }

//    public final void setMaxDurationNano(final long maxDuration) {
//        startChrono();
//        if (maxDuration >= 0) {
//            this.maxTime = getCurrentChronoNano() + maxDuration ;
//        } else {
//            maxTime = -1;
//        }
//    }

    public float getUserTime() {
        return totalTime / 1.0e9F;
    }
    
    public long getUserTimeNano() {
        return totalTime;
    }
}
