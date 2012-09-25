package xerial.core.collection

//--------------------------------------
//
// Enum.scala
// Since: 2012/08/30 9:15
//
//--------------------------------------

/**
 * Enumeration type
 *
 * @author Taro L. Saito
 */
trait Enum[T] {
  def values : IndexedSeq[T]
  def apply(name:String) : T
}
