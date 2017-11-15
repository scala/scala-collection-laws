package laws

/** Information that you can use to filter tests at runtime. */
trait TestInfo {
  def num: Numbers
  def oper: Ops[_, _]
  def inst: Instance[_, _]
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
  protected def boxedRuntime: java.lang.Class[_]
  def runtimeColl: java.lang.Class[_]
}

