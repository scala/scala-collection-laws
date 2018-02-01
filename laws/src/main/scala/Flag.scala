package laws

/** A quality that can be attached to a collection or law to categorize its properties.
  * Flags are used both for general properties of collections (e.g. is it a sequence)
  * and to indicate atypical or buggy behavior for which a test should or should not
  * be run.
  */
final class Flag()(implicit val nm: sourcecode.Name)
extends Ordered[Flag] { 
  override val toString = nm.value.toString
  override def compare(that: Flag) = toString.compareTo(that.toString)
  override def equals(that: Any) = that match {
    case f: Flag => this.toString == that.toString
    case _       => false
  }
  override val hashCode = toString.hashCode
}
object Flag {
  def F(implicit nm: sourcecode.Name) = new Flag()(nm)

  // Fundamental properties of sequences
  val INT = F     // Uses integers as the element type
  val MAP = F     // Is a map
  val SEQ = F     // Is a sequence
  val SET = F     // Is a set
  val STR = F     // Uses strings as the element type

  // Unusual "collections" that are not expected to behave exactly like others
  val ARRAY  = F   // Is an Array
  val STRING = F   // Is a String
  val STRAW  = F   // strawman collections (when used together with regular collections)

  // Buggy behavior for which a fix is immanent
  val CAMEL    = F   // strawman collection is BUGGY on this law!  Straw will break the camel's back :)
  val CAMELMAP = F   // strawmay collectino is BUGGY (specifically for maps).

  // Everything below here is non-ideal but may reflect the best behavior we can get.
  val SUPER_IHASHM  = F  // Some immutable.HashMap operations revert to the supertype
  val SUPER_ITREES  = F  // Some immutable.TreeSet operations revert to the supertype 
  val SUPER_MXMAP   = F  // Some mutable.Map subclass operations always revert to the supertype
  val SUPER_MOPENHM = F  // mutable.OpenHashMap is especially bad with supertype reversion
  val SUPER_ON_ZIP  = F  // Some collections lose their type when zipped (due to ordering or structure)
  
  // Everything down here is _highly_ dubious behavior but is included to get tests to pass
  val ARRAYSTACK_ADDS_ON_FRONT = F  // Bizarre behavior of ArrayStack--it reverses _itself_ when calling result??!
  val PRIORITYQUEUE_IS_SPECIAL = F  // Inconsistent behavior regarding what is dequeued (ordered) vs. not
  val BITSET_MAP_BREAKS_BOUNDS = F  // Because BitSet doesn't allow negative numbers, maps are problematic

  // Workarounds for identified bugs go here.
}
