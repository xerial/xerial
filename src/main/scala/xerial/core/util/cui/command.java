package xerial.core.util.cui;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation for methods that can be invoked as commands
 *
 * @author leo
 *
 */
@Retention(RetentionPolicy.RUNTIME)
@Target( { ElementType.METHOD })
public @interface command {

    /**
     * Description of the option, used to generate a help message of this
     * command-line options.
     */
    String description() default "";

    /**
     * Usage note e.g. "$ command name (argument)"
     * @return
     */
    String usage() default "";
}
