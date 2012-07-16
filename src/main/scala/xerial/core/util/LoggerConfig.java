package xerial.core.util;

import javax.management.MXBean;

/**
 * Logger configuration API
 * @author leo
 */
@MXBean
public interface LoggerConfig {
    public void setLogLevel(String name, String logLevel);
}
