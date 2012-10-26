package xerial.cui

//--------------------------------------
//
// CommandLineLens.scala
// Since: 2012/07/06 4:05 PM
//
//--------------------------------------

import xerial.lens._
import util.matching.Regex
import util.matching.Regex.Match
import collection.mutable.ArrayBuffer
import xerial.core.util.{CommandLineTokenizer, StringTemplate}
import xerial.core.log.Logger

/**
 *
 *
 * @author leo
 */
object CommandLineLens {


}

class CommandLineLens(cl: Class[_]) {

}

/**
 * Creates option parsers
 */
object OptionParser extends Logger {

  def tokenize(line: String): Array[String] = CommandLineTokenizer.tokenize(line)

  def of[A](implicit m: ClassManifest[A]): OptionParser = {
    apply(m.erasure)
  }

  def apply(cl: Class[_]): OptionParser = {
    val schema = new ClassOptionSchema(cl)
    assert(schema != null)
    new OptionParser(schema)
  }

  def newParser[A <: AnyRef](optionHolder: A) = {
    val cl = optionHolder.getClass
    new OptionParser(new ClassOptionSchema(cl))
  }

  def parse[A <: AnyRef](args: Array[String])(implicit m: ClassManifest[A]): A = {
    of[A].build(args)._1
  }

  def parse[A <: AnyRef](argLine: String)(implicit m: ClassManifest[A]): A = {
    parse(tokenize(argLine))
  }

  val defaultUsageTemplate = """usage:$COMMAND$ $ARGUMENT_LIST$
  $DESCRIPTION$
$OPTION_LIST$
"""

}

/**
 * command-line option
 */
sealed abstract class CLOptionItem(val param: Parameter) {

  def takesArgument: Boolean = false

  def takesMultipleArguments: Boolean = {
    import TypeUtil._
    val t: Class[_] = param.valueType.rawType
    isArray(t) || isSeq(t)
  }
}

/**
 * CommandTrait line option and the associated class parameter
 * @param annot
 * @param param
 */
case class CLOption(val annot: option, override val param: Parameter) extends CLOptionItem(param) {
  override def takesArgument: Boolean = !param.valueType.isBooleanType
}

/**
 * CommandTrait line argument type and the associated class parameter
 * @param arg
 * @param param
 */
case class CLArgument(val arg: argument, override val param: Parameter) extends CLOptionItem(param) {
  def name: String = {
    var n = arg.name
    if (n.isEmpty)
      n = param.name
    n
  }
}

/**
 * Schema of the command line options
 */
trait OptionSchema extends Logger {

  val options: Array[CLOption]
  val args: Array[CLArgument] // must be sorted by arg.index in ascending order

  protected lazy val symbolTable = {
    var h = Map[String, CLOption]()
    options.foreach {
      case opt: CLOption =>
        if (!opt.annot.symbol.isEmpty)
          h += opt.annot.symbol -> opt
        if (!opt.annot.alias.isEmpty)
          h += opt.annot.alias -> opt
    }
    h
  }

  def apply(name: String): CLOption = symbolTable.apply(name)

  def findOption(name: String): Option[CLOption] = symbolTable.get(name)
  def findFlagOption(name:String) : Option[CLOption] = {
    findOption(name) filterNot (_.takesArgument)
  }
  def findOptionNeedsArg(name:String) : Option[CLOption] = {
    findOption(name) filter (_.takesArgument)
  }

  def findArgumentItem(argIndex: Int): Option[CLArgument] = {
    if (args.isDefinedAt(argIndex)) Some(args(argIndex)) else None
  }

  def description: String
  def usage: String

  protected def defaultUsage: String = {
    val l = for (a <- args) yield {
      a.name
    }
    l.map("[%s]".format(_)).mkString(" ")
  }

  override def toString = "options:[%s], args:[%s]".format(options.mkString(", "), args.mkString(", "))
}

/**
 * OptionSchema crated from a class definition
 * @param cl
 */
class ClassOptionSchema(val cl: Class[_]) extends OptionSchema {

  private val schema = ObjectSchema(cl)

  val options: Array[CLOption] = {
    //debug("schema of %s:%s", cl.getSimpleName, schema)
    for (p <- schema.constructor.params; opt <- p.findAnnotationOf[option])
    yield new CLOption(opt, p)
  }

  val args: Array[CLArgument] = {
    val argParams = for (p <- schema.constructor.params; arg <- p.findAnnotationOf[argument])
    yield new CLArgument(arg, p)
    argParams.sortBy(x => x.arg.index())
  }

  def description = {
    cl.getDeclaredAnnotations.collectFirst {
      case c: command => c.description
    }.getOrElse("")
  }

  override def usage = {
    cl.getDeclaredAnnotations.collectFirst {
      case c: command if !c.usage.isEmpty => c.usage
    }.getOrElse(defaultUsage)
  }

}

/**
 * OptionSchema created from a method definition
 * @param method
 */
class MethodOptionSchema(method: ObjectMethod) extends OptionSchema {

  val options =
    for (p <- method.params; opt <- p.findAnnotationOf[option]) yield new CLOption(opt, p)

  val args = {
    val l = for (p <- method.params; arg <- p.findAnnotationOf[argument]) yield new CLArgument(arg, p)
    l.sortBy(x => x.arg.index())
  }

  def description = {
    method.jMethod.getDeclaredAnnotations.collectFirst {
      case c: command => c.description
    }.getOrElse("")
  }

  override def usage = {
    val argLine =
      method.jMethod.getDeclaredAnnotations.collectFirst {
        case c: command if !c.usage.isEmpty => c.usage
      }.getOrElse(defaultUsage)

    "%s %s".format(method.name, argLine)
  }

}

/**
 * Option -> value mapping result
 */
sealed abstract class OptionMapping

case class OptSetFlag(opt: CLOption) extends OptionMapping

case class OptMapping(opt: CLOption, value: String) extends OptionMapping

case class OptMappingMultiple(opt: CLOption, value: Array[String]) extends OptionMapping

case class ArgMapping(opt: CLArgument, value: String) extends OptionMapping

case class ArgMappingMultiple(opt: CLArgument, value: Array[String]) extends OptionMapping

class OptionParserResult(val mapping: Seq[OptionMapping], val unusedArgument: Array[String]) {
}

/**
 * CommandTrait-line argument parser
 *
 * @author leo
 */
class OptionParser(val schema: OptionSchema) extends Logger {

  def this(m: ObjectMethod) = this(new MethodOptionSchema(m))

  import OptionParser._

  def build[A](args: Array[String], b: GenericBuilder): OptionParserResult = {
    trace("schema: " + schema)

    val result = parse(args)
    val logger = getLogger("build")
    for (each <- result.mapping) {
      logger.trace("build option: %s", each)
      each match {
        case OptSetFlag(opt) => b.set(opt.param.name, "true")
        case OptMapping(opt, value) => b.set(opt.param.name, value)
        case OptMappingMultiple(opt, value) => {
          value.foreach(v => b.set(opt.param.name, v))
        }
        case ArgMapping(opt, value) => b.set(opt.param.name, value)
        case ArgMappingMultiple(opt, value) => {
          value.foreach(v => b.set(opt.param.name, v))
        }
      }
    }
    result
  }

  /**
   * Build an option holder object from command line arguments
   * @param args
   * @param m
   * @tparam A
   * @return
   */
  def build[A](args: Array[String])(implicit m: ClassManifest[A]): (A, OptionParserResult) = {
    val b = ObjectBuilder(m.erasure)
    val result = build(args, b)
    (b.build.asInstanceOf[A], result)
  }

  /**
   * Parse the command-line arguments
   * @param args
   * @param exitAfterFirstArgument
   * @return parse result
   */
  def parse(args: Array[String], exitAfterFirstArgument: Boolean = false): OptionParserResult = {

    def findMatch[T](p: Regex, s: String) : Option[Match] = p.findFirstMatchIn(s) 

    def group(m: Match, group: Int): Option[String] = {
      if (m.start(group) != -1) Some(m.group(group)) else None
    }



    case class Flag(opt:CLOption, remaining:List[String])
    case class WithArg(opt:CLOption, v:String, remaining:List[String])

    object OptionFlag {
      private val pattern = """^-(\w)""".r

      def unapply(s: List[String]): Option[Flag] = {
        findMatch(pattern, s.head) flatMap { m =>
          val symbol = m.group(1)
          schema.findFlagOption(symbol) map { Flag(_, s.tail) }
        }
      }
    }

    object OptionWithArgument {
      private val pattern = """^-(\w)([:=](\w+))?""".r

      def unapply(s: List[String]): Option[WithArg] = {
        findMatch(pattern, s.head) flatMap { m =>
            val symbol = m.group(1)
            val immediateArg = group(m, 3)
            schema.findOptionNeedsArg(symbol) map { opt =>
              if (immediateArg.isEmpty) {
                if (s.tail.isEmpty)
                  throw new IllegalArgumentException("Option %s needs an argument" format opt)
                else {
                  val remaining = s.tail
                  WithArg(opt, remaining.head, remaining.tail)
                }
              }
              else
                WithArg(opt, immediateArg.get, s.tail)
            }
        }
      }
    }

<<<<<<< HEAD
    def isKnownOption(name: String): Boolean = schema.findOption(name).isDefined

=======
>>>>>>> 618c67beb8175c3b84c83bd74b275f1982a58382
    // Hold mapping, option -> args ...
    val optionValues = collection.mutable.Map[CLOptionItem, ArrayBuffer[String]]()
    val unusedArguments = new ArrayBuffer[String]

    val logger = getLogger("traverse")

    def traverseArg(l: List[String]): Unit = {
      var argIndex = 0

      logger.trace("index:%d, remaining:%s", argIndex, l)

      def appendOptionValue(ci: CLOptionItem, value: String): Unit = {
        val holder = optionValues.getOrElseUpdate(ci, new ArrayBuffer[String]())
        holder += value
      }

      def setArgument(arg: String): Unit = {
        schema.findArgumentItem(argIndex) match {
          case Some(ai) => {
            appendOptionValue(ai, arg)
            if (!ai.takesMultipleArguments)
              argIndex += 1
          }
          case None => unusedArguments += arg
        }
      }

      // Process command line arguments
      var continue = true
      var remaining = l
      while (continue && !remaining.isEmpty) {
        val next = remaining match {
          case OptionFlag(m) => {
            appendOptionValue(m.opt, "true")
            m.remaining
          }
          case OptionWithArgument(m) => {
            appendOptionValue(m.opt, m.v)
            m.remaining
          }
          case e :: rest => {
            setArgument(e)
            if (exitAfterFirstArgument)
              continue = false
            rest
          }
          case Nil => List() // end of arguments
        }
        remaining = next
      }
    }

    traverseArg(args.toList)

    val mapping: Seq[OptionMapping] = {
      val m: TraversableOnce[OptionMapping] = for ((opt, values) <- optionValues) yield {
        opt match {
          case c: CLOption =>
            if (c.takesArgument) {
              if (c.takesMultipleArguments)
                OptMappingMultiple(c, values.toArray)
              else
                OptMapping(c, values(0))
            }
            else
              OptSetFlag(c)
          case a: CLArgument =>
            if (a.takesMultipleArguments)
              ArgMappingMultiple(a, values.toArray)
            else
              ArgMapping(a, values(0))
        }
      }
      m.toSeq
    }
    new OptionParserResult(mapping, unusedArguments.toArray)
  }

  def printUsage = {
    print(createUsage())
  }

  def createOptionHelpMessage = {
    val optDscr: Array[(CLOption, String)] = for (o <- schema.options)
    yield {
      val opt: option = o.annot
      val hasShort = opt.symbol.length != 0
      val hasAlias = opt.alias.length != 0
      val l = new StringBuilder
      if (hasShort) {
        l append "-%s".format(opt.symbol)
      }
      if (hasAlias) {
        if (hasShort)
          l append ", "
        l append "-%s".format(opt.name)
      }

      if (o.takesArgument) {

        if (hasAlias)
          l append ":"
        else if (hasShort)
          l append " "
        l append "[%s]".format(opt.varName)
      }
      (o, l.toString)
    }

    val optDscrLenMax =
      if (optDscr.isEmpty) 0
      else optDscr.map(_._2.length).max

    val defaultInstance: Option[_] = {
      try
        schema match {
          case c: ClassOptionSchema => Some(TypeUtil.newInstance(c.cl))
          case _ => None
        }
      catch {
        case _ => None
      }
    }

    def genDescription(opt: CLOption) = {
      //      if (opt.takesArgument) {
      //        if(defaultInstance.isDefined && defaultInstance.get)
      //        "%s (default:%s)".format(opt.annot.description(),
      //      }
      //      else
      opt.annot.description()
    }

    val s = for (x <- optDscr) yield {
      val paddingLen = optDscrLenMax - x._2.length
      val padding = Array.fill(paddingLen)(" ").mkString
      " %s%s  %s".format(x._2, padding, genDescription(x._1))
    }

    val b = new StringBuilder
    if (!s.isEmpty) {
      b.append("[options]\n")
      b.append(s.mkString("\n") + "\n")
    }
    b.result
  }


  def createUsage(template: String = defaultUsageTemplate): String = {
    StringTemplate.eval(template) {
      Map('ARGUMENT_LIST -> schema.usage, 'OPTION_LIST -> createOptionHelpMessage,
        'DESCRIPTION -> schema.description
      )
    }

  }

}

