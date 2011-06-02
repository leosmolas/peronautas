package massim.framework.util;

/**
 * This exception is thrown whenever some error in configuration is detected.
 * 
 */
public class InvalidConfigurationException extends Exception {

	private static final long serialVersionUID = -1005642894671117236L;

	public InvalidConfigurationException() {
	}

	public InvalidConfigurationException(String message) {
		super(message);
	}

	public InvalidConfigurationException(Exception e) {
		super(e);
	}
}
