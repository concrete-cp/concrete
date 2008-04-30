/*
 * Created on 30 avr. 08
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package cspfj.util;

public class Waker extends Thread {

	final Thread thread;
	final long time;

	public Waker(Thread t, long time) {
		this.thread = t;
		this.time = time;
	}

	public synchronized void run() {
//		for (int i = 1000000000 ; --i>=0;) {
//			if (i%100000 == 0) System.out.println(i);
//		}
		if (time < 0) {
			return;
		}
		try {
			wait(time);
		} catch (InterruptedException e) {
			return; 
		}

		thread.interrupt();
	}
}
