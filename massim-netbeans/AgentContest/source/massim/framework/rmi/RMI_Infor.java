package massim.framework.rmi;
/**
 * RMI_Infor gathers all informations for this rmi server(Document server, statistic server) 
 * and for rmi remote server (flash server)
 * see serverconfig.xml for more details 
 *
 */
public class RMI_Infor {
	public static int RMI_PORT_DEFAULT = 1098;
	public static String RMI_HOST_DEFAULT = "localhost";
	public static String RMI_URL_DEFAULT = "rmi://locahost:1098/";
	/**
	 * this parameter are for flash server. 
	 * flash server is builded for visualization of Massim
	 * 
	 */
	public static boolean FLASH_SERVER_ACTIVATED=false;
	public static int FLASH_PORT = 1099;
	public static String FLASH_SERVER="";
	public static String FLASH_SERVICE ="";
}
