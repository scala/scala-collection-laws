package laws

/** A quality that can be attached to a collection or law to categorize its properties.
  * Flags are used both for general properties of collections (e.g. is it a sequence)
  * and to indicate atypical or buggy behavior for which a test should or should not
  * be run.
  */
final class Flag(val disabled: Boolean = false)(implicit val nm: sourcecode.Name)
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
  def F(implicit nm: sourcecode.Name) = new Flag()(nm)       // This sets a flag
  def X(implicit nm: sourcecode.Name) = new Flag(true)(nm)  // Use this to "comment out" a flag

  // Fundamental properties of sequences
  val INT = F     // Uses integers as the element type
  val MAP = F     // Is a map
  val SEQ = F     // Is a sequence
  val SET = F     // Is a set
  val STR = F     // Uses strings as the element type

  // Unusual "collections" that are not expected to behave exactly like others
  val ARRAY   = F   // Is an Array
  val STRING  = F   // Is a String
  val STRAW   = F   // strawman collections (when used together with regular collections)
  val ORDERLY = F   // Collection is sorted, but can't maintain itself with all operations as it might lose its ordering
  val ONCE    = F   // Collection is consumed on traversal
  val INDEF   = F   // Collection's size is not yet fixed (lazy collections)
  val SPECTYPE= F   // Collection has constraints on element type, which makes some operations not work
  val BITSET  = F   // Collection is specificially a bitset (mutable or immutable)
  
  // Everything down here is _highly_ dubious behavior but is included to get tests to pass
  val PRIORITYQUEUE_IS_SPECIAL = F  // Inconsistent behavior regarding what is dequeued (ordered) vs. not

  // Workarounds for identified bugs go here.
  val BITSET_MAP_AMBIG  = F    // Bit maps don't know whether to use StrictOptimized or SortedSet ops for map.
  val BITSET_ZIP_AMBIG  = F    // Same problem with zipping

  // Pure bugs that aren't fixed yet
  val QUEUE_SLIDING     = F    // Queue and ArrayStack will not give you an underfull sliding window (everything else does)
  val PQ_RETURNS_NULL   = F    // Priority Queue can just give null when empty!
  val LEFT_JOIN_DETYPED = F    // Maps and a few other collections don't retain self-type with `++:`
  val LEFT_JOIN_WRONG   = F    // Maps don't implement `++:` the right way: they actually produce different results
  val SORTWITH_MUTATES  = F    // Array-based collections mutate themselves with `sortWith`!
}
