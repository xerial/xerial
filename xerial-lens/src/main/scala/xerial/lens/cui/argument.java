package xerial.lens.cui;

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
     * Description of this argument
     */
    String description() default "";
}
