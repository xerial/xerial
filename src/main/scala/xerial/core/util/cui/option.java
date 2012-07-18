package xerial.core.util.cui;

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
     * symbol of the option. If this symbol is "h", it handles option "-h".
     * The symbol must be a single character.
     *
     */
    String symbol() default "";

    /**
     * name of the option. If this name is "help", it handles option
     * "--help"
     *
     */
    String name() default "";


    /**
     * Variable name used to describe option argument (e.g. --file=VALUE). The
     * default value is capitalized name().
     */
    String varName() default "value";

    /**
     * Description of the option, used to generate a help message of this
     * command-line option.
     */
    String description() default "";

}


