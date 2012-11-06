package xerial.cui;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation for specifying command-line options.
 *
 * @author leo
 *
 */
@Retention(RetentionPolicy.RUNTIME)
@Target( { ElementType.FIELD, ElementType.METHOD, ElementType.PARAMETER })
public @interface option {

    /**
     * Comma-separated list of option prefixes. For example, "-h,--help" handles option "-h" and
     * "--help".
     */
    String prefix();

    /**
     * Description of the option, used to generate a help message of this
     * command-line option.
     */
    String description() default "";

    /**
     * If this option is used to display help messages of the commands, set this value to true.
     */
    boolean isHelp() default false;
}


