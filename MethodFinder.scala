package laws

import language.higherKinds

/** Finds methods for a particular collection.
  */
object MethodFinder {
  import java.lang.reflect.Method
  import java.lang.reflect.Modifier._
  import scala.reflect.runtime.universe._
  
  val dollarMap = 
    """tilde ~ bang ! at @ hash # percent % up ^ amp & times * minus - plus + eq = bslash \\ bar | colon : less < greater > div / qmark ?""".
    split(' ').grouped(2).map(a => ("\\$" + a(0)) -> a(1))
    
  def dedollar(s: String) = (s /: dollarMap)( (si, m) => si.replaceAll(m._1, m._2) )
  
  val objectMethods = (new Object {}).getClass.getMethods.map(_.getName).map(dedollar).toSet

  def apply[T](klass: Class[_], inst: T)(implicit tag: TypeTag[T]) = {
    val taggedNotPublic = tag.tpe.members.
      filter(_.isMethod).
      filter(! _.isPublic).
      map(_.name.decodedName.toString).
      toSet
      
    klass.getMethods.
      filter(m => (m.getModifiers & (STATIC | PRIVATE | PROTECTED)) == 0).
      map(m => dedollar(m.getName)).
      filter(n => !(n contains '$') && !objectMethods(n) && !taggedNotPublic(n)).
      toSet
    match { case x => println(x); x }
  }
}
