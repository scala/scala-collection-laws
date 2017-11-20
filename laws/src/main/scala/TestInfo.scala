package laws

/** Information that you can use to filter tests at runtime. */
trait TestInfo {
  /** The particular numbers used for this test */
  def num: Numbers

  /** The particular operations used for this test */
  def oper: Ops[_, _]

  /** The particular collections and singleton element used for this test */
  def inst: Instance[_, _]

  /** The runtime type of the element. */
  final def runtimeElt: java.lang.Class[_] = {
    val r = boxedRuntime
    if      (r == classOf[java.lang.Integer])       classOf[Int]
    else if (r == classOf[java.lang.Long])          classOf[Long]
    else if (r == classOf[java.lang.Double])        classOf[Double]
    else if (r == classOf[java.lang.Float])         classOf[Float]
    else if (r == classOf[java.lang.Character])     classOf[Char]
    else if (r == classOf[java.lang.Byte])          classOf[Byte]
    else if (r == classOf[java.lang.Short])         classOf[Short]
    else if (r == classOf[scala.runtime.BoxedUnit]) classOf[Unit]
    else                                            r
  }

  /** Subclasses need to implement this method to get the runtime class type correct */
  protected def boxedRuntime: java.lang.Class[_]

  /** The runtime collection type.  (Subclasses implement this explicitly.) */
  def runtimeColl: java.lang.Class[_]
}

