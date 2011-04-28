package massim.framework.util;

import static massim.framework.util.DebugLog.LOGLEVEL_ERROR;
import static massim.framework.util.DebugLog.log;
import massim.framework.connection.AgentCodecProtocolErrorException;

import org.w3c.dom.Element;

public class ConfigurationUtilities {

	@SuppressWarnings("unchecked")
	public static Class getClassFromConfig(String name)
			throws InvalidConfigurationException {
		try {
			return Class.forName(name);
		} catch (ClassNotFoundException e) {
			log(LOGLEVEL_ERROR, "Unable to find class:" + name);
			throw new InvalidConfigurationException(e);
		}
	}

	@SuppressWarnings("unchecked")
	public static <T> T getNewInstanceFromConfig(String name)
			throws InvalidConfigurationException {
		Class cls = getClassFromConfig(name);
		Object result = null;
		try {
			result = cls.newInstance();
		} catch (IllegalAccessException e) {
			log(LOGLEVEL_ERROR, "Unable to access class:" + name);
			throw new InvalidConfigurationException(e);
		} catch (InstantiationException e) {
			log(LOGLEVEL_ERROR, "Unable to instantiate class:" + name);
			throw new InvalidConfigurationException(e);
		}
		return (T) result;
	}

	@SuppressWarnings("unchecked")
	public static <T> T getObjectFromConfig(String name, Element source)
			throws InvalidConfigurationException {
		Object result = null;
		Class cls = getClassFromConfig(name);
		try {
			result = XMLCodec.convertXMLToObject(source, cls);
		} catch (InstantiationException e) {
			log(LOGLEVEL_ERROR, "Unable to instantiate class:" + name);
			throw new InvalidConfigurationException(e);
		} catch (IllegalAccessException e) {
			log(LOGLEVEL_ERROR, "Unable to access class:" + name);
			throw new InvalidConfigurationException(e);
		} catch (AgentCodecProtocolErrorException e) {
			log(LOGLEVEL_ERROR, "Error while parsing data for class:" + name);
			throw new InvalidConfigurationException(e);
		}
		return (T) result;
	}
}
