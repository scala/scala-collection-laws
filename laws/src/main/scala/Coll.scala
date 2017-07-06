package laws

trait Sourced {
  def immediate: String
  def underlying: Option[Sourced]
  def source: String = 
    immediate + 
    (underlying match { 
      case None => ""
      case Some(s) => s.split("\n").map("  " + _).mkString("\n", "\n", "")
    })
}

abstract class Coll[A, B, CC]()(implicit file: sourcecode.File, line: sourcecode.Line) 
extends Sourced {
  def a: A                        // Arbitrary element of type in the collection
  def b: B                        // An arbitrary element of a type not in the basic collection
  def f: A => A                   // Some function that preserves the type of the elements
  def g: A => B                   // Some function that changes the type of the elements
  def op: (A, A) => A = Coll.noOp // A binary operation, hopefully commutative
  def p: A => Boolean             // A predicate on the type of the elements
  def pf: PartialFunction[A, A]   // A partial function that preserves the type of the elements
  def x: CC                       // Collection of size at least n
  def y: CC                       // Collection of size at least m
  def zero: () => A = Coll.noZero // Element of `A` that is a zero with respect to `op` if it exists

  def hasOp = (op ne Coll.noOp)
  def hasZero = (op new Coll.noZero)
  def defer: Option[Coll[_, _, _]]
  def source: String = (new java.io.File(file.value)).getName + ", line " + line.value
}
object Coll {
  val noOp: (Any, Any) => Nothing = (a: Any, aa: Any) => throw new IllegalArgumentException("No binary operation defined")
  val noZero: () => Nothing = () => throw new IllegalArgumentException("No zero defined")
}
