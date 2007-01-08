package cspfj.filter;

import java.util.Arrays;

public class ArcQueue {
    private int size;

    private int[] arcs;

    private boolean[] inQueue;

    public ArcQueue(int length) {
        arcs = new int[length];
        inQueue = new boolean[length];
        clear() ;
    }
    
    public void clear() {
        size = 0 ;
        Arrays.fill(inQueue, false) ;
    }
    
    public void add(int arc) {
        if (!inQueue[arc]) {
            inQueue[arc] = true ;
            arcs[size++] = arc;
        }
    }
    
    public boolean hasArcs() {
        return size > 0 ;
    }
    
    public int getArc(int i) {
        return arcs[i] ;
    }
    
    public int getSize() {
        return size ;
    }
    
    public void fill() {
        for (int i = arcs.length; --i >= 0;) {
            arcs[i] = i;
        }
        
        Arrays.fill(inQueue, true);
        
        size = arcs.length ;
    }
}
