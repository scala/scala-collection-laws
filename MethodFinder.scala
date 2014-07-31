package laws

import language.higherKinds

object MethodFinder {
  def apply[A](klass: Class[_], inst: TraversableOnce[A]) = klass.getMethods.map(_.getName).toSet
}