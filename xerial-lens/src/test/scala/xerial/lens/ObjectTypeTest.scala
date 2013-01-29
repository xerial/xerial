package xerial.lens

import xerial.core.XerialSpec

//--------------------------------------
//
// PrimitiveTest.scalaSince: 2012/07/17 22:30
//
//--------------------------------------
object ObjectTypeTest {
  case class Person(id:Int, name:String)
}

/**
 * @author leo
 */
class ObjectTypeTest extends XerialSpec {

  "Primitive" should {
    "have all primitives" in {
      val names = (for (each <- Primitive.values) yield {
        each.name
      }).toList

      for(p <- Seq("Boolean", "Short", "Byte", "Char", "Int", "Float", "Long", "Double")) {
        names should (contain (p))
      }
    }

    "have name" in {
      Primitive.Int.name should be("Int")
      Primitive.Float.name should be("Float")
    }
  }

  "ObjectType" should {
    "detect types using Scala 2.10 reflection" in {
      val t = ObjectType(Seq(1, 3, 5))
      t match {
        case SeqType(cl, Primitive.Int) => // OK
        case _ => fail(f"unexpected type: $t")
      }

      val t2 = ObjectType(Seq(Seq(1,2)))
      t2 match {
        case SeqType(cl, SeqType(cl2, Primitive.Int)) => // OK
        case _ => fail(f"unexpected type: $t2")
      }
    }
    import ObjectTypeTest._

    "inspect case classes" in {
      val p = Person(1, "leo")
      val t = ObjectType(p)
      t.name should be ("Person")
    }

    "inspect case classes in Seq" in {
      val t = ObjectType(Seq[Any](Person(1, "leo")))
    }



  }

}