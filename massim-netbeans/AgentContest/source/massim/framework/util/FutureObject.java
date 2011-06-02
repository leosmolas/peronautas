package massim.framework.util;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * This class just wraps some object that will be created or finished later in a Future<T>. It's not cancallable by default.
 *
 * @param <T> class of object to wrap.
 */
public class FutureObject<T> implements Future {
	private T object;
	private boolean delivered;
	/**
	 * Constructs a new FutureObject, which is not "done" by default.
	 *
	 */
	public FutureObject() {
		object=null;
		delivered=false;
	}
	
	/**
	 * won't work.
	 */
	public boolean cancel(boolean arg0) {
		return false;
	}
	/**
	 * This implementation of Future<T> is never cancelled.
	 */
	public boolean isCancelled() {
		return false;
	}
	
	public boolean isDone() {
		return delivered;
	}

	public T get() throws InterruptedException, ExecutionException {
		synchronized(this) {if (!delivered) wait();}
		return object;
	}

	public T get(long timeout, TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
		synchronized(this){if (!delivered) unit.timedWait(this,timeout);}
		return object;
	}

	/**
	 * Deliver object to this FutureObject<T> object. Call this method to assign the object to this Future and
	 * set it to "done". 
	 * @param obj
	 */
	public void deliver(T obj) {
		synchronized(this) {
			object=obj;
			delivered=true;
			notify();
		}
	}
}
