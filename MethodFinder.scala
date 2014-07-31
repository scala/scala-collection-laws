package laws

import language.higherKinds

/** Finds methods for a particular collection.
  * TODO: clean up results.  This is very messy--all sorts of $sp stuff.
  */
object MethodFinder {
  def apply[A](klass: Class[_], inst: TraversableOnce[A]) = klass.getMethods.map(_.getName).toSet
}
