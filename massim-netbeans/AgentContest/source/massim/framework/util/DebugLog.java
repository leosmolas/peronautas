package massim.framework.util;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Calendar;
import java.util.GregorianCalendar;


public class DebugLog {
	public final static int LOGLEVEL_CRITICAL = 400;
	public final static int LOGLEVEL_ERROR = 300;
	public final static int LOGLEVEL_NORMAL = 200;
	public final static int LOGLEVEL_DEBUG = 100;
	
	public static OutputStream os_critical = System.err;
	public static OutputStream os_error = System.err;
	public static OutputStream os_normal = System.out;
	public static OutputStream os_debug = System.out;
	public static int thresholdcritical = 0;
	public static int thresholderror = 0;
	public static int thresholdnormal = 0;
	public static int thresholddebug = 0;
	
	private static synchronized void logString(int type, String s) {
		byte[] b=s.getBytes();
		try {
			if (type>=LOGLEVEL_DEBUG+thresholddebug && type<LOGLEVEL_NORMAL) {
				if (os_debug!=null) os_debug.write(b);
			} else
			if (type>=LOGLEVEL_NORMAL+thresholdnormal && type<LOGLEVEL_ERROR) {
				if (os_normal!=null) os_normal.write(b);
			} else
			if (type>=LOGLEVEL_ERROR+thresholdnormal && type<LOGLEVEL_CRITICAL) {
				if (os_error!=null) os_error.write(b);
			} else
			if (type>=LOGLEVEL_CRITICAL+thresholdcritical) {		
				if (os_critical!=null) os_critical.write(b);
			}
		} catch (IOException e) {
			System.err.println("Error while trying to write log string: "+s);
		}
	}
	private static String getMetaInfo() {
		//Get stack frame
		Exception e = new Exception();
		e.fillInStackTrace();
		StackTraceElement[] stack = e.getStackTrace();
		
		//Get time&date
		GregorianCalendar calendar = new GregorianCalendar();
		
		//Assemble string
		String t = "";
		String x="";
		// 2 means that this method and the calling method are ignored.
		// please note that this implies that this any method that calls getMetaInfo
		// should have been called from outside.
		//for(int i=2;i<stack.length;i++)
		int i=2;
		{ 
			StackTraceElement ls = stack[i];
			t+=x;
			t+= ls.getClassName()+"."+ls.getMethodName()+":"+ls.getLineNumber();
			x="<-";
		}
		String s =
			String.format("%04d%02d%02d %02d:%02d:%02d.%03d",
				calendar.get(Calendar.YEAR), 
				calendar.get(Calendar.MONTH),
				calendar.get(Calendar.DAY_OF_MONTH),
				calendar.get(Calendar.HOUR_OF_DAY),
				calendar.get(Calendar.MINUTE),
				calendar.get(Calendar.SECOND),
				calendar.getTimeInMillis() % 1000) + " " +
			Thread.currentThread().getId() + " " + 
			t;
		return s;
	}

	public static void log(int type) {
		logString(type, getMetaInfo()+"\n");
	}

	public static void log(int type, String s) {
		logString(type, getMetaInfo()+"  "+s+"\n");
	}

}
