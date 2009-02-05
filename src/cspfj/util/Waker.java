package cspfj.util;

import java.util.TimerTask;

public class Waker extends TimerTask {
	private final Thread thread;
	public Waker(Thread thread) {
		this.thread = thread;
	}
	
	
	@Override
	public void run() {
		thread.interrupt();
	}
	
	
}
