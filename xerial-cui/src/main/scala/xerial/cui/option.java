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
     * symbol of the option. If this symbol is "h", it handles option "-h".
     *
     */
    String symbol() default "";

    /**
     * alias of the option. If the alias is "help", it handles option
     * "-help"
     *
     */
    String alias() default "";


    /**
     * Variable name used to describe option argument (e.g. -file:VALUE). The
     * default value is capitalized parameter name.
     */
    String varName() default "";

    /**
     * Description of the option, used to generate a help message of this
     * command-line option.
     */
    String description() default "";

}


