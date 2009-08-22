package cspfj.priorityqueues;

import java.util.Arrays;
import java.util.LinkedList;

public class Fifo<T extends Identified> extends LinkedList<T> {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    private final static int ARRAY_INCREASE = 64;

    private boolean[] inQueue;

    public Fifo(final int initSize) {
        inQueue = new boolean[initSize];
    }

    @Override
    public boolean offer(T e) {
        if (e.getId() >= inQueue.length) {
            inQueue = Arrays.copyOf(inQueue, e.getId() + ARRAY_INCREASE);
        } else if (inQueue[e.getId()]) {
            return false;
        }
        inQueue[e.getId()] = true;
        return super.offer(e);
    }

    @Override
    public T poll() {
        final T data = super.poll();
        inQueue[data.getId()] = false;
        return data;
    }

    public void clear() {
        Arrays.fill(inQueue, false);
        super.clear();
    }

}
