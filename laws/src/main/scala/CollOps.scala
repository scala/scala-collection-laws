package laws


///////////////////////////////////////
// Selection of base collection type //
///////////////////////////////////////

abstract class IntColl[CC](
  values: Values, coll: Provider[Int, CC], act: Active[Int, Long]
)(
  implicit file: sourcecode.File, line: sourcecode.Line
)
extends Coll[Int, Long, CC](values, coll, act)(file, line) {}

abstract class StrColl[CC](
  values: Values, coll: Provider[String, CC], act: Active[String, Option[String]]
)(
  implicit file: sourcecode.File, line: sourcecode.Line
)
extends Coll[String, Option[String], CC](values, coll, act)(file, line) {}


////////////////////////////////////////////////////////////
// Selection of secondary type and mapping from base type //
////////////////////////////////////////////////////////////

object IntFns {
}

trait IntLongG1[CC] { self: IntColl[Long, _] =>
  protected def transform(i: Int): Long = i.toLong + 3
  lazy val b: Long = transform(self.a)
  lazy val g: Int => Long = transform _
}

trait StrOStrG1[CC] { self: StrColl[Option[String], _] =>
  protected def transform(s: String): Option[String] = {
    val i = a
    if ((i eq null) || (i.length < 2)) None else Some(i.substring(1))
  }
  lazy val b: Option[String] = transform(self.a)
  lazy val g: String => Option[String] = transform _
}


///////////////////////////
// Variants on functions //
///////////////////////////

trait IntF1 { self: IntColl[ _, _] => 
  val f: Int => Int = _ + 1
}

trait IntF2 { self: IntColl[_, _] =>
  val f: Int => Int = (i: Int) => (i*i) - 3*i + 1
}

trait StrF1 { self: StrColl[_, _] =>
  val f: String => String = (s: String) => if (s ne null) s.toUpperCase else s
}

trait StrF2 { self: StrColl[_, _] =>
  val f: String => String = (s: String) => if (s ne null) f"<$s<" else s
}


//////////////////////////////////
// Variants on binary operators //
//////////////////////////////////

trait IntOp1 { self: IntColl[_, _] =>
  override val op: (Int, Int) => Int = _ + _
  override val zero: () => Int = () => 0
}

trait IntOp2 { self: IntColl[_, _] =>
  override val op: (Int, Int) => Int = (i: Int, j: Int) => i*j - 2*i - 3*j + 4
}

trait StrOp1 { self: StrColl[_, _] =>
  override val op: (String, String) => String = (s: String, t: String) => {
    if (s eq null) {
      if (t eq null) s else t
    }
    else if (t eq null) s else s + t
  }
  override val zero: () => String = () => ""
}

trait StrOp2 { self: StrColl[_, _] =>
  override val op: (String, String) => String = (s: String, t: String) => {
    if ((s eq null) && (t eq null)) s
    else if (s eq null) t.reverse
    else if (t eq null) s.toUpperCase
    else s.take(t.length) + t.take(s.length).reverse
  }
}


////////////////
// Predicates //
////////////////

trait IntPred1 { self: IntColl[_, _] =>
  override val p: Int => Boolean = i => (i % 3) == 0
}

trait IntPred2 { self: IntColl[_, _] =>
  override val p: Int => Boolean = _ => true
}

trait IntPred3 { self: IntColl[_, _] =>
  override val p: Int => Boolean = _ => false
}

trait StrPred1 { self: StrColl[_, _] => 
  override val p: String => Boolean = s => if (s.length < 2) true else s.charAt(0) > s.charAt(s.length-1)
}

trait StrPred2 { self: StrColl[_, _] =>
  override val p: String => Boolean = _ => true
}

trait StrPred3 { self: StrColl[_, _] =>
  override val p: String => Boolean = _ => false
}

///////////////////////
// Partial Functions //
///////////////////////
