package xerial.core.cui;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;


/**
 * CommandTrait-line argument with no option prefix such as "-" or "--"
 *
 * @author leo
 *
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.FIELD, ElementType.METHOD })
public @interface argument {

    /**
     * Name of this argument. If nothing is given, field name is used;
     */
    String name() default "";

    /**
     * Argument index (0-origin) among the arguments without option prefix, "-"
     * or "--". The default is 0.
     */
    int index() default 0;

    /**
     * Description of this argument
     */
    String description() default "";
}
